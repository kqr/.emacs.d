;;; init.el --- bootstrapping code for init-common.el and init-local.el
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
(require 'package)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setcq use-package-always-ensure t)

;;; ----

;; Find and load existing custom-file since I use customize-set-variable a lot
(setq custom-file "~/.emacs.d/var/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(defmacro setcq (symbol &rest args)
  "A convenient wrapper around (customize-set-variable 'SYMBOL ARGS)."
  (append `(customize-set-variable (quote ,symbol)) args))

;;; ----

;; Location of my themes
(setcq custom-theme-directory "~/.emacs.d/etc/theme/")

;;; ----

;; Where to put temp files
(setcq temporary-file-directory "~/.emacs.d/tmp/")
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

;; Use this as early as possible
(use-package no-littering :init
  ;; Avoid littering in working directory and ~/.emacs.d/ by moving
  ;; temporary/soon to be overwritten files elsewhere
  (setcq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setcq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;;; ----

(load "~/.emacs.d/etc/init-common.el")
(load "~/.emacs.d/etc/init-local.el")

;;; init.el ends here
