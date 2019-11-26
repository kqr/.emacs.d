;;; init.el --- Configuration common to all my Emacs installations
;;
;;; Commentary:
;; For this to be fully functional, Emacs 25 (or 26?) is required.
;;
;; Testing I18N: Räksmörgås
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
;; - olivetti-mode (narrows buffers to fill-column)
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

;; Some things are going to be platform-dependent.
(defconst macos-p (eq system-type 'darwin))
;; set to 8 megabytes (and constantly swap.) default is 800,000 (i.e. 800 kb)
(setq gc-cons-threshold 8000000)

;;; Remove graphical junk.
;; The main purpose of this is to remove many of the distracting graphic
;; components I don't use anyway. Therefore, it might make sense to eval
;; this early to avoid the flickering of said components.
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(width . 145))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'initial-frame-alist '(left-fringe . 20))
(add-to-list 'default-frame-alist '(left-fringe . 20))
(add-to-list 'initial-frame-alist '(right-fringe . 20))
(add-to-list 'default-frame-alist '(right-fringe . 20))

;; Set up some mac-specific graphical junk.
(when macos-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;;; Set various Emacs system defaults.
;; I don't use the customize system anymore, but other packages require it.
(setq custom-file "~/.emacs.d/var/custom.el")
(unless (file-exists-p custom-file)
  (unless (file-directory-p (file-name-directory custom-file))
    (make-directory (file-name-directory custom-file)))
  (write-region "" nil custom-file))
(load-file custom-file)

;; Avoid mixing new fresh code with old bytecompiled code (or vice versa?)
(setq load-prefer-newer t)

;; Keep temp files out of the way.
(setq temporary-file-directory "/tmp/")
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

;;; Configure straight.el, bootstrap it, and install use-package.
(defvar straight-use-package-by-default)
(defvar straight-vc-git-default-protocol)
(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'https)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'diminish)
(straight-use-package 'bind-key)

;;; System-ish packages that should be loaded early.
;; Avoid littering in working directory and ~/.emacs.d/ by moving
;; temporary/soon to be overwritten files elsewhere
;; Require this as early as possible
(use-package no-littering
  :config
  (setq backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; This may help against accidentally running a mix of updated and out-of-date code
(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1))

;; A common problem e.g. on OS X is that Emacs runs in a slightly different
;; environment than what you're used to in the user shell. This should help at
;; least a little with that issue.
(use-package exec-path-from-shell
  :if macos-p
  :config
  (exec-path-from-shell-initialize))

;; Config troubleshooting.
(use-package bug-hunter
  :commands (bug-hunter-init-file))

(use-package cl-lib)

;;; Set up load paths.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lib"))
;; XXX: Remove these two
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc/app"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc/dev"))

;;; User information and misc. config
(setq user-full-name "Christoffer Stjernlöf"
      user-mail-address "a@xkqr.org")

;;; Some "better defaults".
(setq-default
 inhibit-startup-screen t
 initial-scratch-message ""

 major-mode 'fundamental-mode
 make-backup-files nil
 large-file-warning-threshold 100000000

 ;; Reduces lag, I think
 auto-window-vscroll nil
 line-move-visual nil

 ;; Don't complain about not being able to scroll below end of file
 scroll-error-top-bottom t

 ;; Try to keep cursor centered to the extent possible with vanilla Emacs
 scroll-preserve-screen-position t
 scroll-conservatively 0
 maximum-scroll-margin 0.5
 scroll-margin 99999

 ;; No need to fake typesetting.
 sentence-end-double-space nil

 ;; Hide cursor in inactive windows
 cursor-in-non-selected-windows nil

 ;; Set a column limit at 80 characters
 fill-column 80
 ;; Automatically hard wrap content instead
 auto-fill-function 'do-auto-fill
 ;; Create new lines when moving past end of buffer
 next-line-add-newlines t

 ;; Copy stuff to the X11 primary selection
 select-enable-primary t
 ;; Typing stuff with active region replaces region
 delete-selection-mode 1

 ;; Focus on newly opened help windows
 help-window-select t

 ;; Open URLs with Firefox.
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program (if macos-p "open_firefox" "firefox")

 ;; Let text extend beyond the window width
 truncate-lines t

 ;; Prevent Emacs from mixing tabs and spaces.
 indent-tabs-mode nil)

;; Replace the default line-extends-beyond-window symbol
(set-display-table-slot standard-display-table 0 ?›)

;; C-z defaults to suspend-frame which behaves weirdly and is never necessary
(define-key global-map (kbd "C-z") nil)

;; This is neat to quickly go back to the previous buffer
(define-key global-map (kbd "C-q") #'kill-this-buffer)
;; But then we also need this...
(define-key global-map (kbd "C-S-q") #'quoted-insert)

;; Make "join this line to the one above" a bit more convenient to perform
(define-key global-map (kbd "C-S-j") #'delete-indentation)

(when macos-p
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  (setq mac-right-command-modifier 'meta)
  (setq mac-right-option-modifier 'none)
  (setq mac-right-alternate-modifier 'none)

  ;; Set up CUA bindings on OS X. I use these mostly when not in Evil mode, in
  ;; order not to have to context switch between the Vi-style and Emacs-style
  ;; copy/paste bindings, which almost seem to be picked to cause confusion...
  (define-key global-map [(hyper x)] 'kill-region)
  (define-key global-map [(hyper c)] 'copy-region-as-kill)
  (define-key global-map [(hyper v)] 'yank))

;; Set up platform-independent shortcuts
(define-key global-map [(hyper w)] 'delete-frame)
(define-key global-map [(hyper n)] 'make-frame)


;;; Windowing stuff
(defvar f11-map (make-sparse-keymap)
  "Operations related to the interface, e.g. fullscreening,zooming, etc.")
(define-key global-map (kbd "<f11>") f11-map)

;; Never split vertically, but feel free to split horizontally, if there's room.
;; Don't resize windows just because their content changes, though.
(setq split-height-threshold nil
      split-width-threshold 160
      even-window-sizes nil)

;; When making new frame, switch it to scratch buffer
(defun switch-to-scratch-buffer (frame)
  "Switch to scratch buffer in FRAME."
  (with-selected-frame frame
    (switch-to-buffer "*scratch*")))
(push 'switch-to-scratch-buffer after-make-frame-functions)

;; Restore cursor position when reopening file
(save-place-mode +1)

;; Window-divider
(setq window-divider-default-bottom-width 5
      window-divider-default-right-width 5)
(window-divider-mode +1)

;; Enable line numbers.
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode +1)
  (warn "EMACS VERSION < 26 used. If this is unexpected, look into it."))

(use-package ace-window
  :bind ("M-o" . ace-window))

;; Enable quick ways of dragging buffers around
(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right))
  )

(use-package sr-speedbar)

;; Fixed width font
(when (display-graphic-p)
  (set-frame-font (font-spec :name "Input Sans" :size 14) t t)
  (custom-theme-set-faces 'user '(fixed-pitch
                                  ((t :family "Input Mono"
                                      :height 1.0))))  ;; was Luxi Mono, then Hack
  (custom-theme-set-faces 'user '(variable-pitch
                                  ((t :family "Linux Libertine O"
                                      :height 1.3))))
  (add-to-list 'initial-frame-alist '(line-spacing . 1))
  (add-to-list 'default-frame-alist '(line-spacing . 1)))

;; Set variable width font in text buffers
(add-hook 'text-mode-hook 'variable-pitch-mode)

(let ((fallback-font "Noto Sans"))
  (if (not (member fallback-font (font-family-list)))
      (warn (concat fallback-font " not among installed fonts. Symbols may behave weirdly."))
    ;; Noto likely gets much more love in the non-Latin 1 ranges of Unicode.
    (set-fontset-font t nil fallback-font nil 'prepend)
    (set-fontset-font t 'symbol fallback-font)
    (set-fontset-font t 'mathematical fallback-font)))

(use-package face-remap
  :config
  ;; Make available smaller changes in text size
  (setq-default text-scale-mode-step 1.05)
  (define-key f11-map (kbd "f") 'toggle-frame-fullscreen)
  (define-key f11-map (kbd "z") 'text-scale-adjust)

  ;; Set fixed-width fonts where needed
  (setq-default buffer-face-mode-face '(:inherit fixed-pitch))
  (add-hook 'calendar-mode-hook #'buffer-face-mode)
  (add-hook 'notmuch-tree-mode-hook #'buffer-face-mode)
  (add-hook 'dired-mode-hook #'buffer-face-mode)
  (add-hook 'magit-status-mode-hook #'buffer-face-mode)
  (add-hook 'magit-log-mode-hook #'buffer-face-mode))

(use-package ansi-color
  :hook ((compilation-filter . ansi-coloured-buffer))
  :init
  (defun ansi-coloured-buffer ()
    "Interpret ANSI colour sequences correctly in current buffer."
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only)))

;; Dim inactive buffers
(use-package dimmer
  :disabled t
  :config
  (setq dimmer-fraction 0.3)
  (dimmer-mode +1))


(load "kqr-misc.el")

(use-package tramp
  :config
  (defun tramp-file-name-as-hop (vec)
    "Return VEC formatted as a hop."
    (concat (tramp-file-name-hop vec)
            (tramp-file-name-method vec)
            (and (tramp-file-name-method vec)
                 tramp-postfix-method-format)
            (tramp-file-name-user-domain vec)
            (and (tramp-file-name-user-domain vec)
                 tramp-postfix-user-format)
            (tramp-file-name-host-port vec)
            tramp-postfix-hop-format))

  (defun sudo-edit-current-file (as-user)
    (interactive "sAs user? ")
    (let ((position (point)))
      (find-alternate-file
       (if (file-remote-p (buffer-file-name))
           (with-parsed-tramp-file-name buffer-file-name remote
             (tramp-make-tramp-file-name
              "sudo" as-user remote-domain
              remote-host remote-port
              remote-localname
              (tramp-file-name-as-hop
               (tramp-dissect-file-name
                (buffer-file-name)))))
         (concat "/sudo:root@localhost:"
                 (buffer-file-name))))
      (goto-char position))))

;; Replace keywords with Unicode symbols
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun refresh-programming-symbols ()
  "Run this command when prettify-programming-symbols has been re-evaled."
  (interactive)
  (prettify-programming-symbols)
  (global-prettify-symbols-mode -1)
  (global-prettify-symbols-mode +1))

(defun prettify-programming-symbols ()
  "Prettify programming symbols!"
  (interactive)
  (let ((local-symbols
         (pcase major-mode
           ('fsharp-mode '((">>" . ?»)))
           ('csharp-mode '(("=>" . ?⤇)
                           ("foreach" . ?∀)))
           (_ ())))
        (global-symbols
         '(;; Common operators
           ("==" . (?＝ (Bc . Bl) ?＝))  ;; ==
           ("!=" . (?＝ (Bc . Bc) ?/))  ;; =/=
           (">=" . ?≥)
           ("<=" . ?≤)
           ("->" . ?⟼)
           ("<-" . ?⟵)
           ("|>" . ?▷)
           ("<|" . ?◁)
           (">>=" . (?\s (Bl . Bl) ?> (Br . Bc) ?> (Br . Bc) ?=))
           ("<*>" . (?< (Bc . Bl) ?· (Br . Bc) ?>))
           ("*>" . (?· (Br . Bc) ?\s (Bc . Bl) ?>))
           ("<*" . (?< (Br . Bc) ?\s (Bc . Bl) ?·))
           ("<|>" . (?< (Bc . Bl) ?| (Br . Bc) ?>))
           (":>" . (?: (Br . Bc) ?\s (Bc . Bl) ?>))
           ("||" . ?∨)
           ("&&" . ?∧)
           ("!" . ?¬)

           ;; Common types
           ;; Some of these are known under multiple names, but care should be
           ;; taken to only prettify ONE of them per mode, or things get
           ;; confusing. The choice here is strongly influenced by my current
           ;; day job in .NET, mainly C#.
           ("void" . ?∅)
           ("bool" . ?𝔹)  ;; aka boolean, Bool, Boolean – but pick just one
           ("unsigned" . ?ℕ)
           ("int" . ?ℤ)
           ("float" . ?ℝ)
           ("double" . (?ℝ (Br . Bc) ?ℝ))  ;; RR
           ("char" . ?Σ)
           ("string" . (?Σ (tr . cl) ?*))  ;; Σ*

           ;; Greek
           ("alpha" . ?α)
           ("beta" . ?β)
           ("gamma" . ?γ)
           ("Gamma" . ?Γ)
           ("delta" . ?δ)
           ("Delta" . ?Δ)
           ("lambda" . ?λ)
           ("sigma" . ?σ)
           ("Sigma" . ?Σ)
           ("pi" . ?π)
           ("tau" . ?τ)
           ("psi" . ?ψ)
           ("Psi" . ?Ψ)
           ("Phi" . ?Φ))))
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          (append local-symbols global-symbols))))
(add-hook 'prog-mode-hook 'prettify-programming-symbols)
(add-hook 'ess-mode-hook 'prettify-programming-symbols)

;; Allow convenient hotkeys for special characters I use often
(with-eval-after-load "iso-transl"
  (defvar iso-transl-greek-map (make-sparse-keymap))
  (mapc (lambda (mapping)
          (define-key iso-transl-greek-map (kbd (car mapping)) (cdr mapping)))
        '(("a" . "α")
          ("b" . "β")
          ("c" . "χ")
          ("D" . "Δ")
          ("d" . "δ")
          ("e" . "ε")
          ("F" . "Φ")
          ("f" . "φ")
          ("G" . "Γ")
          ("g" . "γ")
          ("H" . "Θ")
          ("h" . "θ")
          ("k" . "κ")
          ("L" . "Λ")
          ("l" . "λ")
          ("m" . "μ")
          ("p" . "π")
          ("r" . "ρ")
          ("S" . "Σ")
          ("s" . "σ")
          ("t" . "τ")
          ("W" . "Ω")
          ("w" . "ω")
          ("x" . "ξ")))
  (define-key global-map (kbd "C-x g") iso-transl-greek-map)

  (define-key global-map (kbd "C-x m") nil)
  (defvar iso-transl-math-map (make-sparse-keymap))
  (mapc (lambda (mapping)
          (define-key iso-transl-math-map (kbd (car mapping)) (cdr mapping)))
        '(("2" . "²")
          ("3" . "³")
          ("g" . "≥")
          ("l" . "≤")
          ("p" . "±")
          ("q" . "√")
          ("d" . "∂")
          ("x" . "×")
          ("i" . "∫")))
  (define-key global-map (kbd "C-x m") iso-transl-math-map)

  (mapc (lambda (mapping)
          (define-key iso-transl-ctl-x-8-map (kbd (car mapping)) (cdr mapping)))
        '((" " . "\ufeff")
          ("." . "…")
          ("m" . "·")
          ("s" . "§")
          ("p" . "¶")))
  )

;; Better buffer browser
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Fancy icons in dired
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after (all-the-icons)
  :diminish all-the-icons-dired-mode
  :config
  (all-the-icons-dired-mode +1))

;; Sidebar based on dired
(use-package dired-sidebar
  :bind ("<C-tab>" . dired-sidebar-jump-to-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix " .")
  (setq dired-sidebar-close-sidebar-on-file-open t))

;; Provide a list of recently opened files
(use-package recentf
  :defer nil
  :after (counsel)
  :bind ("C-x C-r" . counsel-recentf)
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Smart M-x and fuzzy matching everywhere
(use-package smex
  :defer 2)

(use-package ivy
  :defer 2
  :diminish ivy-mode
  :config
  (setq ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ;; Allow input fragments in arbitrary order
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode +1))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c i" . counsel-imenu)))

;; Org-like outlining of ANY document, not only Org files
(use-package outshine
  :hook ((outline-minor-mode . outshine-mode)
         (prog-mode . outline-minor-mode))
  :bind (:map outline-minor-mode-map
              ("M-n" . outshine-narrow-to-subtree)
              ("M-h" . widen))
  :config
  (setq-default outshine-startup-folded-p t)

  ;; Allow narrowing to subtree even when inside subtree
  (define-advice outshine-narrow-to-subtree
      (:before (&rest args) narrow-to-subtree-when-inside-subtree)
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1))))

;; Evil mode
;; XXX: Use-packageify this
(load "kqr-evil.el")

;; Aggressive auto indentation, let's try it again!
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (electric-indent-mode -1)

  (defun cancel-aggressive-indent-timers ()
    (interactive)
    (let ((count 0))
      (dolist (timer timer-idle-list)
        (when (eq 'aggressive-indent--indent-if-changed (aref timer 5))
          (incf count)
          (cancel-timer timer)))
      (when (> count 0)
        (message "Cancelled %s aggressive-indent timers" count))))
  (run-with-timer 60 60 'cancel-aggressive-indent-timers)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c++-mode) (derived-mode-p 'csharp-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))

  (mapc (lambda (mode) (add-to-list 'aggressive-indent-excluded-modes mode))
        ;; Not too surprisingly, the languages to include are the ones with
        ;; significant whitespace.
        '(fsharp-mode haskell-mode python-mode scala-mode))

  ;; Try tweaking this for performance. Default 0.05.
  (setq aggressive-indent-sit-for-time 0.1)

  (global-aggressive-indent-mode +1))

;; Completion with company mode (hopefully practically intrusion-free)
;; This might not work the way I want to because it IS intrusive. Disabled until
;; I have had time to fix it.
(use-package company
  :disabled
  :defer 6
  :diminish company-mode
  :config
  (global-company-mode +1)

  (defun company-complete-common-or-selected ()
    "Insert the common part, or if none, complete using selection."
    (interactive)
    (when (company-manual-begin)
      (if (not (equal company-common company-prefix))
          (company--insert-candidate company-common)
        (company-complete-selection))))

  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))
  ;; Setting 0 idle delay was a nice idea, but it's way too slow for that
  ;; Someone reports 0.3 working okay with omnisharp too, but let's ramp
  ;; up slightly more slowly, to get maximum mileage out of it too.
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)


  ;; We don't want completion to prevent us from actually navigating the code
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "<up>") nil)
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "<down>") nil)
  (define-key company-active-map (kbd "C-<up>") #'company-select-previous)
  (define-key company-active-map (kbd "C-<down>") #'company-select-next)
  (define-key company-active-map (kbd "TAB")
    #'company-complete-common-or-cycle))

(use-package company-posframe
  :after (company)
  :config
  (company-posframe-mode +1))

;;; Miscellaneous interaction
;; Export window contents to neat HTML
(use-package htmlize
  :config
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

;; Analyse command usage frequency to optimise config
(use-package keyfreq
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;;; General editing
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (defun use-reasonable-fill-column-in-text-mode (props)
    (when (derived-mode-p 'text-mode)
      (let ((max-line-length
             (string-to-number (gethash 'max_line_length props "80"))))
        (when (< 120 max-line-length)
          (puthash 'max_line_length "120" props)))))

  (defun apply-indentation-to-all-modes (props)
    (let ((indent-size (string-to-number (gethash 'indent_size props "4"))))
      (setq c-basic-offset indent-size
            js-indent-level indent-size
            js2-basic-offset indent-size
            sgml-basic-offset indent-size)))

  (editorconfig-mode +1)

  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

  (dolist (override '(use-reasonable-fill-column-in-text-mode
                      apply-indentation-to-all-modes))
    (add-hook 'editorconfig-hack-properties-functions
              override)))

;; Visual regexp (on steroids!)
(use-package visual-regexp
  :bind (("C-\%" . vr/query-replace)
         ("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)))

(use-package visual-regexp-steroids
  :after (visual-regexp)
  :config
  (setq sr/default-regexp-modifiers '(:I t :M nil :S nil :U nil)))

;; Expand-region
(use-package expand-region
  :bind ("M-SPC" . er/expand-region))

;; Something I need to get into the habit of using more.
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
  :config
  (setq yas-indent-line 'fixed)
  (yas-global-mode +1))

;; Copy-and-pasting on steroids.
(use-package auto-yasnippet
  :after (yasnippet))

;; Latex editing.
(progn
  ;; TODO verify what's required for this that I'm missing
  ;;(require 'tex-site nil)

  (setq-default font-latex-deactivated-keyword-classes
                '("textual" "type-command" "type-declaration"))

  (defun configure-latex ()
    "Configure AUCTeX in mode hooks."
    (setq-default TeX-PDF-mode t)
    (setq-default TeX-engine 'xetex)
    (setq-default TeX-auto-save t)
    (require 'latex-preview-pane)
    (latex-preview-pane-mode))

  (add-hook 'LaTeX-mode-hook #'configure-latex))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode +1)
  (drag-stuff-define-keys)

  ;; Org already provides convenient dragging of headlines, which is more
  ;; important than this. However, I would still like drag-stuff-mode when point
  ;; is not at a headline, but don't have time to implement that at the moment.
  (with-eval-after-load "org"
    (add-hook 'org-mode-hook (lambda () (drag-stuff-mode -1)))))

;;; Programming
(use-package flycheck
  :hook ((prog-mode . global-flycheck-mode))
  :diminish flycheck-mode
  :config
  ;; Removed idle-change from below to test whether C# performance gets better
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.1
        flycheck-idle-change-delay 2.0
        flycheck-php-phpcs-executable "/usr/local/bin/phpcs"
        flycheck-phpcs-standard "psr2"
        flycheck-python-pycompile-executable "python3"))

;; Subword-mode allows word motions to TerminateInCamelCasedWords
(add-hook 'prog-mode-hook 'subword-mode)

;; Try out dumb-jump for finding definitions etc.
(use-package dumb-jump
  :bind ("C-c ." . dumb-jump-go)
  :config
  ;; Use a popup near point to select location
  (setq dumb-jump-selector nil)
  (with-eval-after-load "csharp-mode"
    (define-key csharp-mode-map (kbd "C-c .") nil)))

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors '("#ff7328" "#f99759" "#f2a06d" "#eaa472"))
  (setq hl-paren-background-colors 'nil)

  (global-highlight-parentheses-mode +1))

(use-package smartparens
  :defer 5
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-<right>" . sp-slurp-hybrid-sexp)
              ("M-s" . sp-split-sexp)
              ("M-r" . sp-join-sexp))
  :config
  (remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)
  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  (sp-use-smartparens-bindings)
  (sp-local-pair 'ada-mode "'" nil :actions nil)
  (sp-local-pair 'fsharp-mode "'" nil :actions nil)
  (sp-local-pair 'js-jsx-mode "<" ">" :actions nil))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook ((prog-mode text-mode) . ws-butler-mode))

(use-package whitespace-mode
  :bind ("C-z" . whitespace-mode)
  :config
  (setq-default whitespace-style '(face trailing tabs spaces newline space-mark tab-mark newline-mark))

  (setq-default whitespace-display-mappings
		'((space-mark 32 [183] [46])
		  (tab-mark 9 [187 9] [92 9])
		  (newline-mark 10 [182 10]))))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1))

(use-package undo-tree
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode +1)
  (setq-default undo-tree-visualizer-diff t))

(use-package smerge-mode
  :config
  (defun sm-try-smerge ()
    "Start smerge-mode automatically when a git conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  (add-hook 'find-file-hook 'sm-try-smerge t))

(use-package ediff
  :config
  (setq-default
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   ediff-diff-program "diff"
   ediff-diff-options "-w")

  (defun ediff-outline-show-all ()
    (when (or (eq major-mode 'org-mode)
              (eq major-mode 'outline-mode)
              outline-minor-mode)
      (outline-show-all)))
  (add-hook 'ediff-prepare-buffer-hook #'ediff-outline-show-all)

  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (add-hook
   'ediff-keymap-setup-hook
   (lambda () (define-key ediff-mode-map "J" 'ediff-copy-both-to-C))))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package projectile
  :bind-keymap ("<f8>" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind (:map projectile-command-map
              ("f" . counsel-projectile)
              ("s G" . counsel-git-grep))
  :config
  (counsel-projectile-mode +1))

;; XXX: Use-packagify these
;; (load "kqr-cc.el")
;; (load "kqr-web.el")
;; (load "kqr-ada.el")
;; (load "kqr-r.el")
;; (load "kqr-lisp.el")
;; (load "kqr-java.el")
;; (load "kqr-haskell.el")
;; (load "kqr-config-mgmt.el")
;; (load "kqr-dotnet.el")
;; (load "kqr-python.el")

(load "kqr-eshell.el")

(use-package timeclock
  ;; Since switching to hledger for time budgeting, these are the commands I
  ;; still use – none of the show time remaining in status bar stuff.
  :bind (("C-x t i" . timeclock-in)
         ("C-x t o" . timeclock-out)
         ("C-x t r" . timeclock-reread-log))
  :config
  (setq timeclock-file "~/org/log.timeclock")
  (setq timeclock-workday 28800))

(use-package calc
  :bind ("<f12>" . calc)
  :config
  (setq calc-display-trail t
	calc-simplify-mode 'units))

(use-package sunrise-commander
  :bind ("<f2>" . sunrise-cd)
  :config
  (setq sr-show-file-attributes nil))

(use-package git-timemachine
  :commands (git-timemachine-toggle)
  :hook (git-timemachine-mode . evil-emacs-state))

(use-package magit
  :bind ("<f3>" . magit-status)
  :config
  ;; Enable all subcommands and options everywhere
  (setq transient-default-level 7)

  (setq magit-log-margin '(t age magit-log-margin-width t 10))
  (setq magit-diff-refine-hunks 'all)
  ;; I used to try to set magit-log-arguments and magit-diff-arguments here,
  ;; but I have since learned that those are set in etc/transient/values.el!
  ;; And I pretty much always want to use those unless otherwise specified.
  (setq magit-use-sticky-arguments nil)

  ;; Restore visibility of foldable sections when opening again
  (setq magit-section-cache-visibility nil)

  ;; Backup uncommitted WIP changes to prevent serious foot-shooting
  (magit-wip-mode +1)

  (setq magit-no-confirm '(safe-with-wip)))

(load "kqr-org.el")
(use-package epresent
  :hook (epresent-mode . evil-emacs-state)
  :config
  ;; Reset default text styles to be similar to regular operation.
  ;; If I want to increase text size when presenting, I can use text-scale for that.
  (setq epresent-text-scale (face-attribute 'default :height))
  (set-face-attribute 'epresent-heading-face nil
                      :height (face-attribute 'org-level-1 :height)
                      :weight 'bold)
  (set-face-attribute 'epresent-subheading-face nil
                      :height (face-attribute 'org-level-2 :height)
                      :weight 'unspecified)

  ;; Undefine the default keys to enter epresent – I use them for other things!
  ;; (If these directives do not suffice, I'll have to add them to the org hook.)
  (define-key org-mode-map (kbd "<f5>") nil)
  (define-key org-mode-map (kbd "<f12>") nil)

  (define-advice epresent--get-frame
      (:around (actual-get-frame &rest args) epresent-frame-set-buffer)
    (apply actual-get-frame args)
    (switch-to-buffer epresent--org-buffer)
    epresent--frame)

  (define-advice redraw-display
      (:around (actual-redraw &rest args) epresent-only-redisplay-frame)
    (if (eq major-mode 'epresent-mode)
        (apply 'redraw-frame (cons nil args))
      (apply actual-redraw args))))

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
            mml-mode
            outline-minor-mode
            latex-preview-pane-mode)))
  (clean-modeline)

  ;; outline-minor-mode is really persistent
  (add-hook 'outline-minor-mode-hook 'clean-modeline)
  (add-hook 'latex-preview-pane-mode-hook 'clean-modeline))

(load "init-local.el" 'noerror)

(provide 'init)
;;; init.el ends here
