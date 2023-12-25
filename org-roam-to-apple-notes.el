;;; org-roam-to-apple-notes.el -- attempt to export org-roam nodes to Apple Notes

;; Copyright (C) 2023 Charl P. Botha

;; Author: Charl P. Botha <cpbotha@vxlabs.com>
;; Version: 1.0
;; Package-Requires: ((org-roam "2.2.2") (htmlize "1.57"))

;; Keywords: org, org-roam, apple notes, applescript
;; URL: https://github.com/cpbotha/org-roam-to-apple-notes



;; without this, code blocks will not be syntax highlighted!
(require 'htmlize)
(require 'org-roam)


;; copied from org-mac-link -- this works better for this than the built-in do-applescript
(defun oran--org-mac-link-do-applescript (script)
  (let (start cmd return)
    (while (string-match "\n" script)
      (setq script (replace-match "\r" t t script)))
    (while (string-match "'" script start)
      (setq start (+ 2 (match-beginning 0))
            script (replace-match "\\'" t t script)))
    (setq cmd (concat "osascript -e '" script "'"))
    (setq return (shell-command-to-string cmd))
    (concat "\"" (org-trim return) "\"")))


(defun oran--maybe-truncate-70 (text)
  ;; if text is longer than 70 characters, truncate to 70 and append the ellipsis …
  (if (> (length text) 70)
      (concat (substring text 0 70) "…")
    text))



(defun oran--note-with-title-exists (title)
  (string= "\"true\""(oran--org-mac-link-do-applescript
                      (concat
                       "tell application \"Notes\"\n"
                       "tell folder \"org-roam\"\n" 
                       "return (note named \"" (oran--maybe-truncate-70 title) "\" exists)\n"
                       "end tell\n"
                       "end tell\n"))))

(defun oran--slugify (title)
  (replace-regexp-in-string " " "-" (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" title))))

;; advice :around when we want to export HTML with absolute paths to local images
(defun oran--abs-img-src-advice (orig-fun source attributes info)
  ;; source is a relative pathname; convert to absolute
  (let ((source-absolute (expand-file-name source)))
    (funcall orig-fun source-absolute attributes info)))

;; advice :around org-html--format-image to export local images as embedded base64
;; inspired by 
;; https://www.reddit.com/r/orgmode/comments/7dyywu/creating_a_selfcontained_html/
(defun oran--base64-img-src-advice (orig-fun source attributes info)
  ;; source is a relative pathname; convert to absolute
  (let ((source-b64 (format "data:image/%s;base64,%s"
                            (or (file-name-extension source) "")
                            (base64-encode-string
                             (with-temp-buffer
                               (insert-file-contents-literally source)
                               (buffer-string))))))
    (funcall orig-fun source-b64 attributes info)))


(defun oran--export-node-to-apple-notes (node temp-dir &optional do-apple-notes abs-img-paths-or-base64)
  "Export the given node as markdown and save it in a file.

If ABS-IMG-PATHS-OR-BASE64 is non-nil, export with absolute paths to local images. The default (nil) is to export with embedded base64 images.
"
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (point (org-roam-node-point node))
         (title (org-roam-node-title node))
         (file-mtime (org-roam-node-file-mtime node))
         ;; silence "Need absolute ‘org-attach-id-dir’ to attach in buffers without filename" error
         (org-attach-directory temp-dir)
         )

    (with-temp-buffer
      ;; we used to use (buffer (find-file-noselect file)) but then the user is left with
      ;; ALL of their org-roam node containing files open
      (insert-file-contents file)
      (save-excursion
        ;; move to the correct point for the export to start
        (goto-char point)
        (add-hook 'org-pandoc-after-processing-html5-hook 'pandoc-hook)
        (message "----- default directory %s" default-directory)

        (advice-add 'org-html--format-image :around (if abs-img-paths-or-base64  #'oran--abs-img-src-advice #'oran--base64-img-src-advice))

        (let ((html (org-export-as 'html (org-at-heading-p) nil 't))
              (oran-html-fn (expand-file-name (concat (oran--slugify title) ".html") temp-dir)))

          (advice-remove 'org-html--format-image (if abs-img-paths-or-base64  #'oran--abs-img-src-advice #'oran--base64-img-src-advice))

          (with-temp-buffer 
            (insert html)
            
            (goto-char (point-min))

            ;; make sure we have title as the first line
            (insert "<h1>" title "</h1>\n")
            (insert "<p>Source modified: "(format-time-string "%FT%T%z" file-mtime) "</p>\n\n")

            ;; so our empty line -> <br> will catch empty lines we added here
            (goto-char (point-min))
            
            ;; (while (re-search-forward "^\\[WARNING\\] .*" nil t)
            ;;   (replace-match ""))
            ;; replace any start of paragraph elems <p> with <br><p> because Notes
            ;; only adds vertical space for <br>
            ;; (while (re-search-forward "<p>" nil t)
            ;;   (replace-match "<br><p>"))

            ;; replace any blank (empty) lines with <br>.  the built-in html
            ;; exported adds blank lines at the right spots, but apple notes
            ;; ignores them unless we add <br>
            (while (re-search-forward "^$" nil t)
              (replace-match "<br>"))

            ;; write contents of current buffer to file in temp-dir
            (write-file oran-html-fn)

            (when do-apple-notes
              ;; get size of oran-html-fn on disk
              (setq oran-html-size (nth 7 (file-attributes oran-html-fn)))
              (message "oran-html-size: %s" oran-html-size)
              (message "creating the note! %s" oran-html-fn)

              ;; note the use of «class utf8» to read the file as UTF-8 else e.g. € is mangled
              (oran--org-mac-link-do-applescript
               (concat
                "set BODY_FN to (the POSIX path of \"" oran-html-fn "\")\n"
                "set NBODY to read BODY_FN as «class utf8»\n"
                "tell application \"Notes\"\n"
                ;;"activate\n"
                "tell folder \"org-roam\"\n"
                "if not (note named \"" (oran--maybe-truncate-70 title) "\" exists) then\n"
                "make new note with properties {body:NBODY}\n"
                "else\n"
                "set existingNote to note named \"" title "\"\n"
                "set body of existingNote to NBODY\n"
                "end if\n"
                "end tell\n"
                "end tell\n")))

            ;; if HTML export was successful, we return the full filename
            oran-html-fn))))))

;;;###autoload
(defun oran-export-org-roam-nodes-to-apple-notes (&optional update-existing)
  "Export all of the org-roam nodes into the org-roam folder in Apple Notes.

To save time, this will only export nodes that do not yet exist
in Apple Notes (by title) unless UPDATE-EXISTING is non-nil, in
which case it will also update the contents of the nodes that
already exist."
  (interactive)
  (let ((temp-dir (make-temp-file "org-roam-" t))
        (node-list (org-roam-node-list)))
    (message "======> exporting %d nodes" (length node-list))
    (dolist (node node-list)
      (if (and (oran--note-with-title-exists (org-roam-node-title node)) (not update-existing))
          (message "note already exists, skipping: %s" (org-roam-node-title node))
        (progn
          (message "exporting /updating %s" (org-roam-node-title node))
          ;; catch error from export, report, but continue to next node
          (condition-case err
              (oran--export-node-to-apple-notes node temp-dir 't)
            (error (message "ERROR exporting %s: %s" (org-roam-node-title node) err))))))
    (message "DONE.")))

;;;###autoload
(defun oran-export-org-roam-nodes-to-temp-dir ()
  "Export all org-roam nodes to a temporary directory and return the temp-dir path."
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
  "Export the currently active org-roam node to the temp directory as HTML."
  (interactive)
  (oran--export-node-to-apple-notes (org-roam-node-at-point) temporary-file-directory nil))

;;;###autoload
(defun oran-export-this-node-to-apple-notes ()
  "Export the currently active org-roam node to the org-roam folder in Apple Notes."
  (interactive)
  (oran--export-node-to-apple-notes (org-roam-node-at-point) temporary-file-directory 't))


(provide 'org-roam-to-apple-notes)

;;(org-roam-node-file-mtime (org-roam-node-at-point))
