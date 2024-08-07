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
(fringe-mode 20)  ; The fringes are used to carry information

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
;;
;; Maybe it happens also with other platforms, so try running it everywhere.

(use-package exec-path-from-shell
  :config
  ;; No point doing this on windows...
  (unless (eq system-type 'windows-nt)
    (exec-path-from-shell-initialize)))

;; Config troubleshooting.
(use-package bug-hunter
  :commands (bug-hunter-init-file))

(use-package cl-lib)

;;; Set up load paths.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/etc"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lib"))

;;; User information
(setq user-full-name "Christoffer Stjernlöf"
      user-mail-address "a@xkqr.org")

;;; Some "better defaults".
(setq-default
 inhibit-startup-screen t
 initial-scratch-message ""

 major-mode 'fundamental-mode
 make-backup-files nil
 large-file-warning-threshold 100000000

 ;; Count a buffer as unused if it is saved and not viewed in the last 3 hours.
 ;; (This is safe(r) because I don't run clean-buffer-list on a schedule – I
 ;; generally invoke it manually when I feel finished with something and am
 ;; moving on to a completely new thing.)
 clean-buffer-list-delay-general 0.125

 ;; Reduces lag, I think
 auto-window-vscroll nil
 line-move-visual nil

 ;; Don't complain about not being able to scroll below end of file
 scroll-error-top-bottom t

 ;; No need to fake typesetting.
 sentence-end-double-space nil

 ;; Hide cursor in inactive windows
 cursor-in-non-selected-windows nil

 ;; Set a column limit at 80 characters
 fill-column 80
 ;; Automatically hard wrap content instead
 auto-fill-function 'do-auto-fill

 ;; Copy stuff to the X11 primary selection
 select-enable-primary t

 ;; Focus on newly opened help windows
 help-window-select t

 ;; Open URLs with Firefox.
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program (if macos-p "open_firefox" "firefox")

 ;; Let text extend beyond the window width
 truncate-lines t

 ;; Prevent Emacs from mixing tabs and spaces.
 indent-tabs-mode nil)

;; Try to keep cursor centered to the extent possible with vanilla Emacs
(setq-default
 scroll-preserve-screen-position t
 scroll-conservatively 0
 maximum-scroll-margin 0.5
 scroll-margin 99999)

(defun set-scroll-margin-to-half-window-height (fn &rest args)
  "Set 'scroll-margin' to half window height before calling FN with ARGS."
  (let ((scroll-margin (/ 2 (window-height))))
    (apply fn args)))

;; Advise evil-scroll functions with more sensible scroll-margin since they
;; use it to compute how much to scroll.
(with-eval-after-load "evil"
  (advice-add 'evil-scroll-down :around 'set-scroll-margin-to-half-window-height)
  (advice-add 'evil-scroll-up :around 'set-scroll-margin-to-half-window-height))

;; Replace the default line-extends-beyond-window symbol
(set-display-table-slot standard-display-table 0 ?›)

;; C-z defaults to suspend-frame which behaves weirdly and is never necessary
(define-key global-map (kbd "C-z") nil)

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



;; Enable quick ways of dragging buffers around
;; I used to have buffer-move here but it's rare enough that I need to move
;; buffers around that it makes little sense to me to have dedicated support for it.

;; Fixed width font
(when (and (display-graphic-p)
           (find-font (font-spec :name "Hack"))
           (find-font (font-spec :name "Linux Libertine")))
  (set-frame-font (font-spec :name "Hack" :size 14) t t)
  (custom-theme-set-faces 'user '(fixed-pitch
                                  ((t :family "Hack"
                                      :height 1.0))))  ;; was Luxi Mono, then Hack
  (custom-theme-set-faces 'user '(variable-pitch
                                  ((t :family "Linux Libertine"
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

;;; Early packages that Org depends on but that do not depend on Org
;; (hopefully...)

(use-package undo-tree
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
  (global-undo-tree-mode +1)
  (setq-default undo-tree-visualizer-diff t)
  ;; I don't need persistent history and this might slow bulk file operations down.
  (setq undo-tree-auto-save-history nil))

;; Contains remove-all-advice-for which kqr-org relies on.
(load "kqr-misc.el")

;;; Load Org
;; This needs to be called very early, to prevent the incorrect default Org
;; version to be partially loaded.
(load "kqr-org.el")

;;; More misc stuff

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
           )))
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
  (run-at-time nil (* 5 60) 'recentf-save-list)

  ;; recentf-save-list calls write-file which calls save-buffer which prints a
  ;; message when called "interactively." Suppress this message.
  (define-advice recentf-save-list
      (:around (impl &rest args) recentf-save-silently)
    (let ((save-silently t))
      (apply impl args))))

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
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
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
          (cl-incf count)
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

;;; Miscellaneous interaction
;; Export window contents to neat HTML
(use-package htmlize
  :config
  (setq htmlize-output-type 'inline-css)

  ;; Automatically upload HTML of region-or-buffer to remote
  (defvar htmlize-paste-it-target-directory "/-:kqr@two-wrongs.com:/var/www/pastes/")
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
  ;; This doesn't work well with multiple sessions running simultaneously.
  ;; I also don't use it very often. Let's disable it for now.
  (keyfreq-mode -1)
  (keyfreq-autosave-mode +1))

;;; General editing

;; Jump around, to different windows and everything!
;; For more inspiration, see https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :config
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

;; List files and headings and definitions in a sidebar
(use-package sr-speedbar
  :bind (:map speedbar-mode-map
              ("Q" . sr-speedbar-close)
              ("q" . sr-speedbar-close))
  :hook (sr-speedbar-mode . evil-emacs-state)
  :init
  (defun sr-speedbar-open-and-goto ()
    (interactive)
    (sr-speedbar-open)
    (sr-speedbar-select-window))
  :config
  (evil-define-key '(normal visual) 'global (kbd "<leader>f") 'sr-speedbar-open-and-goto))

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
      (setq-default
       c-basic-offset indent-size
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
  :after (yasnippet)
  :config
  (evil-define-key '(normal visual) 'global (kbd "<leader>y") 'aya-create)
  (evil-define-key '(normal visual) 'global (kbd "<leader>p") 'aya-expand))

;; Spell checking. This is not at all refined yet. There are useful tips:
;; - https://www.emacswiki.org/emacs/FlySpell
;; - https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; Removed because I rarely use it, it confuses me, and it conflicts with the
;; key I have configured for avy.


(use-package drag-stuff
  :config
  (drag-stuff-global-mode +1)
  (drag-stuff-define-keys)

  ;; Org already provides convenient dragging of headlines, which is more
  ;; important than this. However, I would still like drag-stuff-mode when point
  ;; is not at a headline, but don't have time to implement that at the moment.
  (with-eval-after-load "org"
    (add-hook 'org-mode-hook (lambda () (drag-stuff-mode -1)))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)))

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

;; The version I want (1.1) comes with Emacs 28, but just to be sure, get it
;; from source. This is a bit of a hack, but it seems to sidestep the existence
;; of a bundled xref.
(straight-use-package 'xref)
(load "~/.emacs.d/straight/repos/xref/xref.el")

;; Note to self: in evil, gd means go to definition.
(use-package dumb-jump
  :config
  ;; Use a popup near point to select location
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  ;; Add a generic vc-based project-root method. This should be part of
  ;; project.el but I'm not sure why it's not. Also the dumb-jump functionality
  ;; works when Emacs is freshly started, and breaks only when ESS is started. I
  ;; really have no idea what's going on, but this fix seems to work.
  ;;
  ;; Maybe for maximum workage this should be defined in some sort of hook to be
  ;; automatically called when ESS is started, but for the time being I can
  ;; probably run it manually.
  (cl-defmethod project-root ((project (head vc)))
    (cdr project)))

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
  (projectile-mode +1)
  ;;(add-hook 'project-find-functions #'projectile-project-root)
  )

(use-package counsel-projectile
  :after (counsel projectile)
  :bind (:map projectile-command-map
              ("f" . counsel-projectile)
              ("s G" . counsel-git-grep))
  :config
  (counsel-projectile-mode +1))

(use-package cc-mode
  :mode ("\\.\\(c\\|h\\)\\'" . c-mode)
  :config
  (setq-default c-default-style "stroustrup"
                c-basic-offset 4))

(use-package haskell-mode
  :mode ("\\.hs\\'"))

(use-package restclient
  :mode "\\.http\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package cperl-mode
  :mode ("\\.\\(pl\\|pm\\|t\\)\\'" . cperl-mode)
  :config
  (setq cperl-indent-level 4))

;; This now assumes Emacs 27 with its built-in JS mode.
(use-package js2-mode
  :mode ("\\.jsx?\\'" . js-mode)
  :hook ((js-mode . javascript-configuration))
  :init
  (defun javascript-configuration ()
    "Configure four spaces indent and js2 linting for JavaScript."
    (js2-minor-mode +1)
    ;; I would like to set this through editorconfig hacks instead when
    ;; appropriate, but that doesn't appear to work for some reason.
    (setq js2-basic-offset 4)
    (setq js-indent-level js2-basic-offset)
    (setq c-basic-offset js-indent-level)))

(use-package css-mode
  :mode "\\.css\\'")

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq-default web-mode-enable-auto-pairing nil
                web-mode-enable-css-colorization nil
                web-mode-css-indent-offset 2))

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . php-enable-psr2-coding-style))

(use-package ess
  :mode ("\\.r\\'" . ess-r-mode)
  :config
  (require 'ess-site))

;; Emacs Lisp and CL stuff
(progn
  (defun configure-lisp-mode ()
    (setq c-basic-offset 2))

  (add-hook 'emacs-lisp-mode-hook 'configure-lisp-mode)
  (add-hook 'lisp-mode-hook 'configure-lisp-mode))

(use-package python
  :hook (python-mode . python-mode-configure)
  :init
  (defun python-mode-configure ()
    (push 'python-mode aggressive-indent-excluded-modes))
  :config
  (setq python-shell-interpreter "python3"))

(use-package nxml
  :mode "\\..sproj\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

;;; Application type major modes.
(load "kqr-eshell.el")

(use-package calc
  :bind ("<f12>" . calc)
  :config
  (setq calc-display-trail t
	calc-simplify-mode 'units))

(use-package sunrise-commander
  :bind ("<f2>" . sunrise-cd)
  :config
  (setq sr-show-file-attributes nil))

(use-package magit
  :bind ("<f3>" . magit-status)
  :config
  ;; Ensure default key binds work in blame mode, and that it exits into normal state.
  (defun magit-blame-ensure-evil-emacs-state ()
    (if magit-blame-read-only-mode
        (evil-emacs-state)
      (evil-normal-state)))
  (add-hook 'magit-blame-read-only-mode-hook 'magit-blame-ensure-evil-emacs-state)
  ;; Make it easy to bring up magit-file-dispatch
  (evil-define-key '(normal visual) 'global (kbd "<leader>m") 'magit-file-dispatch)

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

;;; Email client
(load "kqr-notmuch.el")

;;; Theming
;; Location of my themes
(setq custom-theme-directory "~/.emacs.d/etc/theme/")

;; We like our theme
(defun reset-theme ()
  "Reset theme to the one we like."
  (setq frame-background-mode 'light)
  (load-theme 'modern-minik t)
  (modern-minik-eval-init))
(reset-theme)

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
