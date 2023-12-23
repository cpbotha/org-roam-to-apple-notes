
(require 'org-roam)
(require 'ox-pandoc)
(require 'org-mac-link)


                                        ; 
(defun export-node-to-apple-notes (node)
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
        (setq oran-html-fn (expand-file-name (plist-get (org-pandoc-export-to-html5 nil (org-at-heading-p) nil nil) 'output-file)))

        ;; wait for pandoc to actually finish
        ;; not even sure if this is necessary anymore.
        ;; I did this because Apple Notes kept on failing to import embedded images,
        ;; and it looked like it was being given incomplete HTML files. However, in the
        ;; end it seems that Apple Notes's applescript support is just really really sad.
        (while (not oran-html-done)
          (sleep-for 0.1))

        ;; get size of oran-html-fn on disk
        (setq oran-html-size (nth 7 (file-attributes oran-html-fn)))
        (message "oran-html-size: %s" oran-html-size)
        
        (message "creating the note! %s" oran-html-fn)
        (org-mac-link-do-applescript
         (concat
          "set BODY_FN to (the POSIX path of \"" oran-html-fn "\")\n"
          "set NBODY to read BODY_FN\n"
          "tell application \"Notes\"\n"
          "activate\n"
          "tell folder \"org-roam\"\n" 
          "if not (note named \"" title "\" exists) then\n"
          "make new note with properties {body:NBODY}\n"
          "end if\n"
          "end tell\n"
          "end tell\n"
          ))


        ))))

(defun note-with-title-exists (title)
  (string= "\"true\""(org-mac-link-do-applescript
                      (concat
                       "tell application \"Notes\"\n"
                       "tell folder \"org-roam\"\n" 
                       "return (note named \"" bleh-title "\" exists)\n"
                       "end tell\n"
                       "end tell\n"
                       
                       
                       ))))

(note-with-title-exists "test name")

;; (export-node-to-apple-notes (org-roam-node-at-point))

;; create a new random temp directory
(let ((default-directory (make-temp-file "org-roam-" t)))

  )



;; create directory /tmp/oman-temp if it does not exist
(unless (file-exists-p "/tmp/oman-temp")
  (make-directory "/tmp/oman-temp"))


