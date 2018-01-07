;;; init.el --- bootstrapping code for init-common.el and init-local.el
;;; Commentary:
;; For this to be fully functional, Emacs 25 is required.  A good internet
;; connection is probably a good idea for the initial setup as well.
;;
;;; Todo:
;; - find a way to list the defines in current buffer?
;;
;;; Code:

;; Find and load existing custom-file since I use customize-set-variable a lot
(setq custom-file "~/.emacs.d/var/custom.el")
(unless (file-exists-p custom-file)
  (unless (file-directory-p (file-name-directory custom-file))
    (make-directory (file-name-directory custom-file)))
  (write-region "" nil custom-file))
(load-file custom-file)

(eval-and-compile (push "~/.emacs.d/etc" load-path))
(require 'kqr-load-path)

;;; Package system

;; Set up the package system
(require 'package)
(mapc (lambda (elt) (push elt package-archives))
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  ;; Bootstrap installation with packages that can't be handled by use-package
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'org))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'setcq)

(setcq use-package-always-ensure t)

;;; Theming

;; Location of my themes
(setcq custom-theme-directory "~/.emacs.d/etc/theme/")

;;; No littering

;; Where to put temp files
(setcq temporary-file-directory "/tmp/")
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

;; Use this as early as possible
(use-package no-littering :init
  ;; Avoid littering in working directory and ~/.emacs.d/ by moving
  ;; temporary/soon to be overwritten files elsewhere
  (setcq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setcq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;;; Load other config
(require 'init-common)
(require 'init-local)

;;; init.el ends here
