;;; kqr-dotemacs --- summary
;;; Commentary:
;; For this to be fully functional, Emacs 25 is required.  A good internet
;; connection is probably a good idea for the initial setup as well.
;;
;;; Todo:
;; - find a way to list the defines in current buffer?
;;
;;; Code:
;;
;; Set up the package system
(package-initialize)

;; Find and load existing custom-file since I use customize-set-variable a lot
(setq custom-file "~/.emacs.d/config/local/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Location of my themes
(customize-set-variable 'custom-theme-directory "~/.emacs.d/themes/")

;; Local config directory loader
(defun load-directory (dir)
  "Load all *.el files located in DIR."
  (let ((load-it
	 (lambda (f)
           (progn (message "Applying config file %s" f)
                  (load-file (concat (file-name-as-directory dir) f))))))
    (mapc load-it (directory-files dir nil "\\.el$"))))



(load-directory "~/.emacs.d/config/")
(load-directory "~/.emacs.d/config/local/")

(setq-default major-mode 'text-mode)
(setq-default make-backup-files nil)
(setq-default large-file-warning-threshold 100000000)

;; You want this because otherwise Emacs will mix tabs and spaces...
(customize-set-variable 'indent-tabs-mode nil)

;;; init.el ends here
