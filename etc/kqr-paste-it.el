(when (require 'htmlize nil)
  (setq htmlize-output-type 'inline-css)

  ;; Automatically upload HTML of region-or-buffer to remote
  (defvar htmlize-paste-it-target-directory "/-:two-wrongs.com:pastes/")
  (defvar htmlize-paste-it-base-url "https://two-wrongs.com/pastes/")

  (defun htmlize-paste-it ()
    "Htmlize region-or-buffer and copy to directory."
    (interactive)
    (let* ((start (if (region-active-p) (region-beginning) (point-min)))
           (end (if (region-active-p) (region-end) (point-max)))
           (basename (file-name-base (buffer-name)))
           (extension (file-name-extension (buffer-name)))
           (hash (sha1 (current-buffer) start end))
           (file-name (concat basename "-" (substring hash 0 6)
                              "." extension ".html"))
           (new-file (concat htmlize-paste-it-target-directory file-name))
           (access-url (concat htmlize-paste-it-base-url file-name)))
      ;; Region messes with clipboard, so deactivate it
      (deactivate-mark)
      (with-current-buffer (htmlize-region start end)
        ;; Copy htmlized contents to target
        (write-file new-file)
        ;; Ensure target can be accessed by web server
        (chmod new-file #o755))
      ;; Put URL into clipboard
      (kill-new access-url))))
