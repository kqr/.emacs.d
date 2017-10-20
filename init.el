;;; kqr-dotemacs --- summary
;;; Commentary:
;;
;; * Requires Emacs 25
;; * Requires an internet connection
;;

;;; Todo:
;; - find a way to browse tags (ggtags etc?)
;; - find a way to list the defines in current buffer?
;;
;; Inspiration for further items:
;; - https://oremacs.com/2015/04/16/ivy-mode/
;; - https://github.com/julienfantin/.emacs.d/blob/master/init.el
;; - https://github.com/magnars/.emacs.d/blob/master/init.el
;; - https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org
;; - https://github.com/rejeep/emacs/
;; - https://github.com/hlissner/doom-emacs
;; - https://github.com/redguardtoo/emacs.d
;; - https://github.com/defunkt/emacs
;; - https://github.com/lunaryorn/old-emacs-configuration
;; - https://github.com/grettke/home/blob/master/.emacs.el
;; - https://github.com/abo-abo/oremacs
;; - https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; - https://github.com/grettke/home
;; - https://github.com/jorgenschaefer/Config/blob/master/emacs.el

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
