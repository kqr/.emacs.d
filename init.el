;;; init.el --- Configuration common to all my Emacs installations
;;
;;; Commentary:
;; For this to be fully functional, Emacs 25 (or 26?) is required.
;;
;; Testing I18N: Räksmörgås
;;
;;; Todo:
;; - Set up a more library-like structure of my various configs
;;   - E.g. a minor mode that wraps multiple related minor modes?
;;     See: https://nullprogram.com/blog/2013/02/06/
;;     and: https://stackoverflow.com/questions/23944477/
;;
;; - Find a way to list the defines in current buffer? imenu?
;; - Check out https://github.com/DarthFennec/highlight-indent-guides
;;
;;; Mourned and never forgotten:
;;
;; There are some modes that do great things for my experience, but which have
;; either turned out to be too slow/buggy for everyday use, or I have some
;; suspicion it might be too slow or buggy for everyday use. Some of these are
;; obviously very slow (e.g. indent guides), and others are just slow in
;; specific cirumstances (e.g. aggressive indent).
;;
;; At some point, I wish to start re-introducing these slowly, tweaking them for
;; performance as I go. My configuration for them should be removed in the same
;; commit as I add them to this list, meaning finding my old config should be
;; relatively easy when it's time.
;;
;; - centered-cursor-mode (scrolls windws to show as much above as below point)
;; - olivetti-mode (narrows buffers to fill-column)
;; - aggressive-indent-mode (indents continuously as you type)
;; - fic-mode (clearly highlights TODO, FIXME, XXX, etc. in comments)
;;
;; Until very recently, it was unthinkable for me to remove some of these,
;; because they were the very reason I used Emacs in the first place. What
;; changed my mind was very sage advice from someone in #emacs, who said, "try
;; running the Emacs default for a while, and see how you like it." It's hard to
;; argue against /trying/.
;;
;; Besides, there are other good (and performant!) things about Emacs I can
;; still make use of. I hate the idea of having to shape my usage after the
;; limits of the system (part of me feels like the limitlessness of it all is
;; one of the core reasons to use Emacs in the first place!), but then I realise
;; I am only able to say that because Emacs gives me the luxury of having the
;; choice.
;;
;; That said, some things I am not ready to get rid of:
;;
;; - undo-tree (how in the everliving heck do people survive without this??)
;; - evil-mode (must be replaced with god-mode if removed)
;; - evil-surround-mode (nice and I suspect performant)
;; - evil-cleverparens-mode (known suspect for slowness, but way too important)
;;
;; It will take a lot for me to do anything about the above.
;;
;; There are also some modes that I have suspected to be an issue, but that
;; don't seem to be (issues persist even after running without them for a
;; while.) These are still candidate for removal if issues persist even with the
;; above:
;;
;; - ws-butler-mode
;; - flycheck
;; - yasnippet
;; - highlight-parentheses-mode
;; - prettify-programming-symbols
;; - outshine-mode
;;
;;; Code:
(setq gc-cons-threshold 800000)

;;; Init file configuration
;; I don't use customize-set-variable as much anymore, but it's probably a
;; good idea to load the custom file anyway...
(setq custom-file "~/.emacs.d/var/custom.el")
(unless (file-exists-p custom-file)
  (unless (file-directory-p (file-name-directory custom-file))
    (make-directory (file-name-directory custom-file)))
  (write-region "" nil custom-file))
(load-file custom-file)

;; Set up the package system
(require 'package)
(mapc (lambda (elt) (push elt package-archives))
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ;;("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ;;("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Where to put temp files
(setq temporary-file-directory "/tmp/")
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

;; Avoid littering in working directory and ~/.emacs.d/ by moving
;; temporary/soon to be overwritten files elsewhere
;; Require this as early as possible
(require 'no-littering)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; A common problem e.g. on OS X is that Emacs runs in a slightly different
;; environment than what you're used to in the user shell. This should help at
;; least a little with that issue.
(when (require 'exec-path-from-shell nil)
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lib"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc/app"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc/dev"))

(require 'cl-lib)

(defun load-path-dev-emacs-refresh ()
  "Add any repositories under ~/dev/emacs also to the load path."
  (interactive)
  (let ((dev-emacs (expand-file-name "~/dev/emacs/")))
    (mapc (lambda (repo) (add-to-list 'load-path (expand-file-name repo dev-emacs)))
          (cl-remove-if (lambda (repo) (string-match-p "^\\." repo))
                        (and (file-directory-p dev-emacs)
                             (directory-files dev-emacs))))))
(load-path-dev-emacs-refresh)


;;; User information and misc. config
(setq user-full-name "Christoffer Stjernlöf"
      user-mail-address "a@xkqr.org")

;; Config troubleshooting
(autoload 'bug-hunter-init-file "bug-hunter" nil t)

(defvar f11-map (make-sparse-keymap)
  "Operations related to the interface, e.g. fullscreening,zooming, etc.")
(define-key global-map (kbd "<f11>") f11-map)

;; Better defaults for variables
(load "kqr-defaults.el")
;; UI
(load "kqr-interface.el")
(load "kqr-typography.el")
(load "kqr-print.el")

;; Word count in modeline
(require 'wc-mode)
(define-key global-map (kbd "M-+") 'wc-mode)
(add-hook 'after-save-hook #'wc-reset)
(setq wc-modeline-format "Words:%W%w(%gw)")

(load "kqr-tramp.el")
(load "kqr-unicode.el")

;;;; Navigation and fuzzy finding
;; Better buffer browser
(autoload 'ibuffer "ibuffer")
(define-key ctl-x-map (kbd "C-b") #'ibuffer)

;; Sidebar based on dired
(load "kqr-sidebar.el")

;; Provide a list of recently opened files
;; bind to C-x C-r because I don't use find-file-read-only too much (though I
;; probably should...)
(when (require 'recentf)
  (recentf-mode +1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (define-key global-map (kbd "C-x C-r") #'counsel-recentf))

;; Smart M-x and fuzzy matching everywhere
(run-with-idle-timer
 2 nil
 (lambda ()
   (require 'smex)
   (when (require 'ivy)
     (diminish 'ivy-mode)
     (setq-default ivy-initial-inputs-alist nil)
     (ivy-mode +1)

     (autoload 'counsel-M-x "counsel")
     (define-key global-map (kbd "M-x") #'counsel-M-x))))

;; Org-like outlining of ANY document, not only Org files
(load "kqr-outshine.el")

;; Evil mode
(load "kqr-evil.el")

;; Aggressive auto indentation, let's try it again!
(load "kqr-indentation.el")

;; Completion with company mode (hopefully practically intrusion-free)
;;(load "kqr-company.el")

;;;; Miscellaneous interaction
;; Export window contents to neat HTML
(load "kqr-paste-it.el")

;; Analyse command usage frequency to optimise config
(when (require 'keyfreq nil)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; General editing
(load "kqr-editorconfig.el")

;; Visual regexp (on steroids!)
(autoload 'vr/query-replace "visual-regexp")
(autoload 'vr/isearch-forward "visual-regexp")
(autoload 'vr/isearch-backward "visual-regexp")
(define-key global-map (kbd "C-\%") 'vr/query-replace)
(define-key global-map (kbd "C-s") 'vr/isearch-forward)
(define-key global-map (kbd "C-r") 'vr/isearch-backward)
(eval-after-load "visual-regexp"
  '(progn
     (require 'visual-regexp-steroids nil)
     (setq-default sr/default-regexp-modifiers '(:I t :M nil :S nil :U nil))))


;; Expand-region
(autoload 'er/expand-region "expand-region")
(define-key global-map (kbd "M-SPC") 'er/expand-region)


;; Thesaurus/synonyms tooltip
(define-key global-map (kbd "C-@") #'synosaurus-choose-and-replace)
(autoload 'synosaurus "synosaurus-choose-and-replace")
(eval-after-load "synosaurus"
  '(setq-default synosaurus-choose-method 'popup))

(load "kqr-snippets.el")
(load "kqr-latex.el")

;;; Programming
(load "dev-defaults.el")
(load "kqr-parens.el")
(load "kqr-whitespace.el")
(load "kqr-revisions.el")


;;;; Project management
(autoload 'projectile-command-map "projectile")
(define-key global-map (kbd "<f8>") 'projectile-command-map)
(with-eval-after-load "projectile"
  (when (executable-find "uctags")
    (setq projectile-tags-command "uctags -Re -f \"%s\" %s"))
  (projectile-mode +1)
  (when (require 'counsel-projectile nil)
    (counsel-projectile-mode +1))

  (defun load-two-wrongs ()
    (let ((two-wrongs-file (concat (projectile-project-root) "two-wrongs.el")))
      (when (file-exists-p two-wrongs-file)
          (load two-wrongs-file))))

  (add-hook 'projectile-find-file-hook #'load-two-wrongs))

(load "kqr-cc.el")
(load "kqr-web.el")
(load "kqr-ada.el")
(load "kqr-r.el")
(load "kqr-lisp.el")
(load "kqr-java.el")
(load "kqr-haskell.el")
(load "kqr-config-mgmt.el")
(load "kqr-dotnet.el")
(load "kqr-python.el")

;;; Time reporting, clocking etc
(load "kqr-timeclock.el")
(load "kqr-calculator.el")
(load "kqr-fileman.el")
(load "kqr-git.el")
(load "kqr-org.el")
(load "kqr-epresent.el")

;;; Email client
(load "kqr-notmuch.el")

;;; Theming
;; Location of my themes
(setq custom-theme-directory "~/.emacs.d/etc/theme/")

;; We like our theme
(setq frame-background-mode 'dark)
(load-theme 'modern-minik t)
(modern-minik-eval-init)

;; But we also don't want a bunch of junk in the modeline...
(when (require 'diminish nil)
  (setq eldoc-minor-mode-string "")
  (defun clean-modeline ()
    (mapc #'diminish
          '(auto-fill-function
            abbrev-mode
            auto-revert-mode
            yas-minor-mode
            aggressive-indent-mode
            mml-mode
            outline-minor-mode
            latex-preview-pane-mode
            editorconfig-mode
            ws-butler-mode
            olivetti-mode)))
  (clean-modeline)

  ;; outline-minor-mode is really persistent
  (add-hook 'outline-minor-mode-hook 'clean-modeline)
  (add-hook 'latex-preview-pane-mode-hook 'clean-modeline))

(load "init-local.el" 'noerror)

(provide 'init)
;;; init.el ends here
