;;; org-roam-to-apple-notes.el -- attempt to export org-roam nnodes to Apple Notes

;; Copyright (C) 2023 Charl P. Botha

;; Author: Charl P. Botha <cpbotha@vxlabs.com>
;; Version: 1.0
;; Package-Requires: ((org-roam "2.2.2"))
;; Keywords: org, similarity
;; URL: https://github.com/cpbotha/org-roam-to-apple-notes

;; This file is DEPRECATED. I'm leaving it here in case it helps anyone else
;; choosing the ox-pandoc way. I initially spent quite an amount of time due to
;; pandoc's embed-resources feature, but in the end the built-in org-html
;; exported, with some advice to do the base64 embedding, did a much better job
;; over-all.

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

(defun oran--slugify (title)
  (replace-regexp-in-string " " "-" (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" title))))


(defun oran--export-node-to-apple-notes (node temp-dir do-apple-notes)
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

         ;; we embed images as base64 in the forlorn hope that Apple Notes might show these
         ;; https://emacs.stackexchange.com/questions/27060/embed-image-as-base64-on-html-export-from-orgmode
         ;; --self-contained is a deprecated synonym for --embed-resources --standalone but
         ;; --embed-resources not yet supported by ox-pandoc
         (org-pandoc-options-for-html5 '((standalone . t) (self-contained . t)))
         )

    (setq oran-html-done nil)
    (setq oran-pandoc-buffer nil)
    ;; this hook will be called when the pandoc process actually returns
    ;; in theory, the HTML output should be complete
    (defun pandoc-hook ()
      ;; this is the best place to get the pandoc buffer
      (setq oran-pandoc-buffer (current-buffer))
      (setq oran-html-done t)
      )

    
    (with-current-buffer buffer
      (save-excursion
        ;; move to the correct point for the export to start
        (goto-char point)
        (add-hook 'org-pandoc-after-processing-html5-hook 'pandoc-hook)
        (message "----- default directory %s" default-directory)
        ;; a s v b e
        ;; a=ignored, subtreep, visible, body-only, ext-plist
        ;; URGH org-pandoc-export-as- can only push to a *pandoc <N>* buffer
        ;; however, by setting default-directory, to-html5 creates file where we want
        ;; with plist-get, we get the effective output filename
        ;;(setq oran-html-fn (expand-file-name (plist-get (org-pandoc-export-to-html5 nil (org-at-heading-p) nil nil) 'output-file)))

        ;; a s v b e
        ;; a=ignored, subtreep, visible, body-only, ext-plist
        (org-pandoc-export-as-html5 nil (org-at-heading-p) nil nil)

        ;; wait for pandoc to actually finish
        ;; can take quite long for the new buffer to appear, hence the 30s timeout. sorry.
        (setq wait-time 0)
        (while (and (not oran-html-done) (< wait-time 30))
          (message "waiting for pandoc: %.1fs" wait-time)
          ;; update display, and be interruptible by input, instead of sleep-for
          (sit-for 0.1)
          ;; if pandoc errors out, pandoc-hook is never called, and pandoc also
          ;; sever raises an error or anything (only message), so we have to
          ;; have our own little timeout as well
          (setq wait-time (+ wait-time 0.1)))

        (if oran-pandoc-buffer
            (with-current-buffer oran-pandoc-buffer
              (let ((oran-html-fn (expand-file-name (concat (oran--slugify title) ".html") temp-dir)))

                ;; remove any "[WARNING] some long message\n" lines from top of buffer
                (goto-char (point-min))
                (while (re-search-forward "^\\[WARNING\\] .*" nil t)
                  (replace-match ""))
                ;; replace any start of paragraph elems <p> with <br><p> because Notes
                ;; only adds vertical space for <br>
                (while (re-search-forward "<p>" nil t)
                  (replace-match "<br><p>"))

                ;; write contents of current buffer to file in temp-dir
                (write-file oran-html-fn)

                ;; kill current buffer
                (kill-buffer (current-buffer))
                
                (when do-apple-notes
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
                    "end tell\n")))
                ;; if HTML export was successful, we return the full filename
                oran-html-fn))
          nil)))))

;;;###autoload
(defun oran-export-org-roam-nodes-to-apple-notes ()
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
              (oran--export-node-to-apple-notes node temp-dir 't)
            (error (message "ERROR exporting %s: %s" (org-roam-node-title node) err))))))
    (message "DONE.")))

;;;###autoload
(defun oran-export-org-roam-nodes-to-temp-dir ()
  (interactive)
  (let ((temp-dir (make-temp-file "org-roam-" t))
        (node-list (org-roam-node-list)))
    (message "======> exporting %d nodes" (length node-list))
    (dolist (node node-list)
      (message "exporting %s" (org-roam-node-title node))
      ;; catch error from export, report, but continue to next node
      (condition-case err
          (oran--export-node-to-apple-notes node temp-dir nil)
        (error (message "ERROR exporting %s: %s" (org-roam-node-title node) err))))
    (message "DONE.")
    temp-dir))

;;;###autoload
(defun oran-export-this-node-to-tmp ()
  (interactive)
  (oran--export-node-to-apple-notes (org-roam-node-at-point) "/tmp" nil))

;;;###autoload
(defun oran-export-this-node-to-apple-notes ()
  (interactive)
  (oran--export-node-to-apple-notes (org-roam-node-at-point) "/tmp" 't))


(provide 'org-roam-to-apple-notes)

;;(org-roam-node-file-mtime (org-roam-node-at-point))
