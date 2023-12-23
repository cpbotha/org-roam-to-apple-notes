;;; org-roam-to-apple-notes.el -- attempt to export org-roam nnodes to Apple Notes

;; Copyright (C) 2023 Charl P. Botha

;; Author: Charl P. Botha <cpbotha@vxlabs.com>
;; Version: 1.0
;; Package-Requires: ((org-roam "2.2.2"))
;; Keywords: org, similarity
;; URL: https://github.com/cpbotha/org-roam-to-apple-notes


(require 'org-roam)
(require 'ox-pandoc)
;; for org-mac-link-do-applescript, built-in do-applescript fails in mysterious ways
(require 'org-mac-link)

(defun oran--note-with-title-exists (title)
  (string= "\"true\""(org-mac-link-do-applescript
                      (concat
                       "tell application \"Notes\"\n"
                       "tell folder \"org-roam\"\n" 
                       "return (note named \"" title "\" exists)\n"
                       "end tell\n"
                       "end tell\n"))))
                                        ; 
(defun oran--export-node-to-apple-notes (node temp-dir)
  "Export the given node as markdown and save it in a file."
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (point (org-roam-node-point node))
         (title (org-roam-node-title node))
         (file-mtime (org-roam-node-file-mtime node))
         ;; read file into buffer, but re-use buffer if it already exists 
         (buffer (find-file-noselect file))
         ;; silence "Need absolute ‘org-attach-id-dir’ to attach in buffers without filename" error
         (org-attach-directory "/tmp")

         ;; https://emacs.stackexchange.com/questions/27060/embed-image-as-base64-on-html-export-from-orgmode
         ;; we need self-contained to include images as base64
         (org-pandoc-options-for-html5 '((standalone . t) (self-contained . t)))
         )

    (setq oran-html-done nil)
    ;; this hook will be called when the pandoc process actually returns
    ;; in theory, the HTML output should be complete
    (defun pandoc-hook ()
      (setq oran-html-done t)
      )

    
    (with-current-buffer buffer
      (save-excursion
        ;; move to the correct point for the export to start
        (goto-char point)
        (add-hook 'org-pandoc-after-processing-html5-hook 'pandoc-hook)
        ;; a s v b e
        ;; a=ignored, subtreep, visible, body-only, ext-plist
        ;; URGH org-pandoc-export-as- can only push to a *pandoc <N>* buffer
        ;; however, by setting default-directory, to-html5 creates file where we want
        ;; with plist-get, we get the effective output filename
        (let ((default-directory temp-dir))
          (message "----- default directory %s" default-directory)
          (setq oran-html-fn nil)
          (setq oran-html-fn (expand-file-name (plist-get (org-pandoc-export-to-html5 nil (org-at-heading-p) nil nil) 'output-file))))

        ;; wait for pandoc to actually finish
        ;; not even sure if this is necessary anymore.
        ;; I did this because Apple Notes kept on failing to import embedded images,
        ;; and it looked like it was being given incomplete HTML files. However, in the
        ;; end it seems that Apple Notes's applescript support is just really really sad.
        ;; TBF, before this wait, I did see the HTML export success message AFTER the new note was generated
        (setq wait-time 0)
        (while (and (not oran-html-done) (< wait-time 3))
          (sleep-for 0.1)
          ;; if pandoc errors out, pandoc-hook is never called, and pandoc also
          ;; sever raises an error or anything (only message), so we have to
          ;; have our own little timeout as well
          (setq wait-time (+ wait-time 0.1)))

        (if (file-exists-p oran-html-fn)
            (progn
              ;; get size of oran-html-fn on disk
              (setq oran-html-size (nth 7 (file-attributes oran-html-fn)))
              (message "oran-html-size: %s" oran-html-size)
              
              (message "creating the note! %s" oran-html-fn)
              (org-mac-link-do-applescript
               (concat
                "set BODY_FN to (the POSIX path of \"" oran-html-fn "\")\n"
                "set NBODY to read BODY_FN\n"
                "tell application \"Notes\"\n"
                ;;"activate\n"
                "tell folder \"org-roam\"\n" 
                "if not (note named \"" title "\" exists) then\n"
                "make new note with properties {body:NBODY}\n"
                "end if\n"
                "end tell\n"
                "end tell\n"
                )))
          nil)


        ))))

;;;###autoload
(defun oran-export-org-roam-nodes ()
  (interactive)
  (let ((temp-dir (make-temp-file "org-roam-" t))
        (node-list (org-roam-node-list)))
    (message "======> exporting %d nodes" (length node-list))
    (dolist (node node-list)
      (if (oran--note-with-title-exists (org-roam-node-title node))
          (message "note already exists: %s" (org-roam-node-title node))
        (progn
          (message "exporting %s" (org-roam-node-title node))
          ;; catch error from export, report, but continue to next node
          (condition-case err
              (oran--export-node-to-apple-notes node temp-dir)
            (error (message "ERROR exporting %s: %s" (org-roam-node-title node) err))))))
    (message "DONE.")))



(provide 'org-roam-to-apple-notes)

;;(org-roam-node-file-mtime (org-roam-node-at-point))
