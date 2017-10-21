;;; kqr-dotemacs --- summary
;;; Commentary:
;;
;; * Requires Emacs 25
;; * Requires an internet connection
;;

;;; Todo:
;; - find a way to browse tags (ggtags etc?)
;; - find a way to list the defines in current buffer?

;;; Code:

(defun load-directory (dir)
  "Load all *.el files located in DIR."
  (let ((load-it
	 (lambda (f)
           (progn (message "Applying config file %s" f)
                  (load-file (concat (file-name-as-directory dir) f))))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(package-initialize)

(setq custom-file "~/.emacs.d/config/local/custom.el")
(load custom-file)

(customize-set-variable 'custom-theme-directory "~/.emacs.d/themes/")
(load-directory "~/.emacs.d/config/")
(load-directory "~/.emacs.d/config/local/")

(setq-default major-mode 'text-mode)
(setq-default make-backup-files nil)
(setq-default large-file-warning-threshold 100000000)

;; You want this because otherwise Emacs will mix tabs and spaces...
(customize-set-variable 'indent-tabs-mode nil)

;;; init.el ends here
