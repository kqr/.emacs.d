;;; modern-minik-theme --- A sparsely but tastefully coloured light/dark theme.
;;; Commentary:
;;
;; I've used a syntax highlighting theme similar to this since the beginning of
;; time.  The underlying idea is that syntax highlighting
;; *isn't actually that great*.  It may help with discerning the code
;; structure, but beyond that it is simply in the way and distracting you from
;; reading the code.
;;
;; This is why only strings and comments (and lately, keywords and type names)
;; get their own highlighting colour.
;;
;; Other than that, important text should use the default face, and less
;; important text should use a darker gray to disappear into the shadows.
;;
;;; Code:

(deftheme modern-minik
  "A sparsely but tastefully coloured dark theme.")

(defun from-faces-map (faces-map)
  "Convert FACES-MAP into the correct faces spec for theme-set-faces."
  (apply #'append
         (mapcar (lambda (face-assoc)
                   (let ((face-spec (car face-assoc))
                         (real-faces (cdr face-assoc)))
                     (mapcar (lambda (real-face)
                               (list real-face face-spec))
                             real-faces)))
                 faces-map)))

(defun buffer-status-indicator ()
  "Indicate buffer status with letters instead of symbols."
  (if (buffer-local-value buffer-read-only (window-buffer))
      "(read-only) "
    (if (buffer-modified-p)
        "(***) "
      " ")))

(let* ((light? (equal 'light frame-background-mode))
       (theme-default-color (if light? "black" "#e6dfd3"))
       (theme-background-color (if light? "#fbf1e4" "#141414"))
       (theme-strong-highlight "antiquewhite3")
       (theme-weak-highlight (if light? "antiquewhite2" "#222222"))
       (theme-error-color "red")
       (theme-primary-accent (if light? "chocolate3" "#E8B964"))
       (theme-secondary-accent (if light? "dodgerblue3" "#6493e8"))
       (theme-strong-diminuitive (if light? "forest green" "#7abc71"))
       (theme-weak-diminuitive "peachpuff4")

       (theme-faces
        (from-faces-map
         `(
           ;; Set the default face, which is inherited by most things
           (((t :background ,theme-background-color :foreground ,theme-default-color))
            default)

           ;; Set these without the background, so we can inherit from them
           (((t :foreground ,theme-default-color))
            variable-pitch
            fixed-pitch)

           (((t :background ,theme-default-color :foreground ,theme-default-color))
            cursor)

           ;; These are odd ones, otherwise default+bold is rarely used
           (((t :weight bold))
            undo-tree-visualizer-active-branch-face)

           ;; Region defines our "inverse video" style
           (((t :inverse-video t))
            region)

           ;; The mode-line is a lighter type of inverse video
           (((t :foreground ,theme-weak-diminuitive
                :background ,theme-weak-highlight))
            mode-line)

           (((t :background ,theme-weak-highlight))
            show-paren-match
            highlight)

           (((t :foreground ,theme-error-color
                :weight bold))
            error)

           (((t :underline (:color ,theme-error-color :style wave)))
            flycheck-error)

           (((t :foreground ,theme-primary-accent))
            font-lock-builtin-face)

           (((t :underline (:color ,theme-primary-accent :style wave)))
            flycheck-warning)

           (((t :foreground ,theme-primary-accent
                :weight bold))
            font-lock-keyword-face)

           (((t :foreground ,theme-primary-accent
                :inverse-video t))
            match)

           (((t :foreground ,theme-secondary-accent))
            font-lock-type-face)

           (((t :box (:line-width 1 :color ,theme-background-color :style pressed-button)))
            org-code)

           (((t :underline (:color ,theme-secondary-accent :style wave)))
            flycheck-info)

           (((t :foreground ,theme-secondary-accent
                :underline t))
            link)

           (((t :foreground ,theme-secondary-accent
                :inverse-video t))
            lazy-highlight)

           (((t :foreground ,theme-strong-diminuitive))
            font-lock-string-face)

           (((t :foreground ,theme-weak-diminuitive))
            font-lock-comment-face)))))

  (setq
   theme-faces
   (append
    theme-faces
    '(
      (isearch                               ((t :inherit match)))
      (query-replace                         ((t :inherit match)))
      (isearch-fail                          ((t :inherit error)))
      (show-paren-mismatch                   ((t :inherit error)))

      (window-divider                        ((t :inherit mode-line :inverse-video t)))
      ;; Not sure I'm a huge fan of this, but will try it out for a while anyway
      (window-divider-first-pixel            ((t :foreground "#333333")))
      (window-divider-last-pixel             ((t :foreground "#333333")))
      (fringe                                ((t :inherit highlight)))
      (mode-line-inactive                    ((t :inherit font-lock-comment-face)))
      (secondary-selection                   ((t :inherit mode-line)))

      (escape-glyph                          ((t :inherit org-code :height 0.7)))

      (widget-field                          ((t :inherit mode-line)))
      (button                                ((t :inherit link)))

      (minibuffer-prompt                     ((t :inherit font-lock-keyword-face)))

      (warning                               ((t :inherit font-lock-builtin-face)))

      (ivy-current-match                     ((t :inherit match)))
      (ivy-minibuffer-match-face-1           ((t :inherit fixed-pitch)))
      (ivy-minibuffer-match-face-2           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-3           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-4           ((t :inherit lazy-highlight)))
      (ivy-cursor                            ((t :inherit cursor)))
      (ivy-match-required-face               ((t :inherit isearch-fail)))

      (undo-tree-visualizer-default-face     ((t :inherit font-lock-comment-face)))
      (undo-tree-visualizer-current-face     ((t :inherit warning)))
      (undo-tree-visualizer-unmodified-face  ((t :inherit font-lock-string-face)))

      (org-level-1                           ((t :inherit fixed-pitch
                                                 :height 1.2
                                                 :weight bold)))
      (org-level-2                           ((t :inherit fixed-pitch
                                                 :height 1.1
                                                 :slant italic)))
      (org-level-3                           ((t :inherit fixed-pitch)))
      (org-level-4                           ((t :inherit fixed-pitch)))
      (org-level-5                           ((t :inherit fixed-pitch)))
      (org-level-6                           ((t :inherit fixed-pitch)))
      (org-level-7                           ((t :inherit fixed-pitch)))
      (org-level-8                           ((t :inherit fixed-pitch)))
      (org-level-9                           ((t :inherit fixed-pitch)))

      (org-document-title                    ((t :inherit fixed-pitch :weight bold)))
      (org-document-info                     ((t :inherit fixed-pitch :slant italic)))

      (org-table                             ((t :inherit fixed-pitch)))
      (org-code                              ((t :inherit fixed-pitch)))
      (org-block                             ((t :inherit fixed-pitch)))
      (org-block-background                  ((t :inherit fixed-pitch)))
      (org-link                              ((t :inherit link)))
      (org-footnote                          ((t :inherit font-lock-comment-face)))
      (org-special-keyword                   ((t :inherit font-lock-comment-face)))

      (org-ellipsis                          ((t :inherit font-lock-keyword-face)))
      (org-todo                              ((t :inherit font-lock-keyword-face)))
      (org-done                              ((t :inherit font-lock-comment-face)))
      (org-agenda-done                       ((t :inherit font-lock-comment-face)))
      (org-upcoming-deadline                 ((t :inherit font-lock-keyword-face)))
      (org-scheduled                         ((t :inherit font-lock-comment-face)))
      (org-scheduled-today                   ((t :inherit fixed-pitch)))
      (org-scheduled-previously              ((t :inherit warning)))
      (org-priority                          ((t :inherit font-lock-type-face)))
      (org-agenda-structure                  ((t :inherit font-lock-type-face)))
      (org-agenda-date                       ((t :inherit fixed-pitch)))
      (org-agenda-date-today                 ((t :inherit fixed-pitch)))
      (org-agenda-date-weekend               ((t :inherit fixed-pitch)))
      (org-warning                           ((t :inherit error)))

      (font-lock-constant-face               ((t :inherit fixed-pitch)))
      (font-lock-variable-name-face          ((t :inherit fixed-pitch)))
      (font-lock-doc-face                    ((t :inherit font-lock-comment-face)))
      (font-lock-function-name-face          ((t :inherit fixed-pitch :slant italic)))
      (font-lock-warning-face                ((t :inherit flycheck-warning)))
      (jdee-font-lock-public-face            ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-private-face           ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-protected-face         ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-modifier-face          ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-package-face           ((t :inherit fixed-pitch)))
      (jdee-font-lock-number-face            ((t :inherit fixed-pitch)))
      (jdee-font-lock-constant-face          ((t :inherit fixed-pitch)))
      (jdee-font-lock-constructor-face       ((t :inherit fixed-pitch)))

      (js2-function-param                    ((t :inherit fixed-pitch)))
      (js2-function-call                     ((t :inherit fixed-pitch)))
      (js2-external-variable                 ((t :inherit flycheck-warning)))
      (js2-warning                           ((t :inherit flycheck-warning)))

      (fsharp-ui-operator-face               ((t :inherit fixed-pitch)))

      (sh-heredoc                            ((t :inherit font-lock-string-face)))
      (ess-backquoted-face                   ((t :inherit fixed-pitch)))

      (shm-current-face                      ((t :inherit show-paren-match)))
      (shm-quarantine-face                   ((t :inherit fixed-pitch)))

      (outshine-level-1                      ((t :inherit font-lock-comment-face)))
      (outshine-level-2                      ((t :inherit font-lock-comment-face)))
      (outshine-level-3                      ((t :inherit font-lock-comment-face)))
      (outshine-level-4                      ((t :inherit font-lock-comment-face)))

      (whitespace-hspace                     ((t :inherit fixed-pitch)))
      (whitespace-newline                    ((t :inherit fixed-pitch)))
      (whitespace-tab                        ((t :inherit fixed-pitch)))
      (whitespace-space                      ((t :inherit fixed-pitch)))
      (whitespace-trailing                   ((t :inherit font-lock-keyword-face)))
      (column-enforce-face                   ((t :inherit error)))

      (fic-face                              ((t :inherit error)))
      (fic-author-face                       ((t :inherit font-lock-keyword-face)))
      (flycheck-fringe-info                  ((t :inherit font-lock-string-face)))
      (flycheck-fringe-warning               ((t :inherit warning)))
      (flycheck-fringe-error                 ((t :inherit error)))

      (message-header-subject                ((t :inherit fixed-pitch)))
      (message-cited-text                    ((t :inherit font-lock-string-face)))
      (message-header-cc                     ((t :inherit font-lock-comment-face)))
      (message-header-name                   ((t :inherit font-lock-comment-face)))
      (message-header-other                  ((t :inherit font-lock-comment-face)))
      (message-header-xheader                ((t :inherit font-lock-comment-face)))
      (message-separator                     ((t :inherit font-lock-comment-face)))
      (message-header-to                     ((t :inherit font-lock-comment-face)))
      (message-mml                           ((t :inherit font-lock-comment-face)))

      (notmuch-search-matching-authors       ((t :inherit warning)))
      (notmuch-search-non-matching-authors   ((t :inherit warning)))
      (notmuch-tag-face                      ((t :inherit font-lock-type-face)))
      (notmuch-message-summary-face          ((t :inherit font-lock-comment-face)))
      (notmuch-search-count                  ((t :inherit font-lock-comment-face)))
      (notmuch-search-date                   ((t :inherit font-lock-comment-face)))
      (notmuch-wash-cited-text               ((t :inherit font-lock-string-face)))
      (notmuch-tree-match-author-face        ((t :inherit font-lock-comment-face)))
      (notmuch-tree-match-date-face          ((t :inherit font-lock-comment-face)))
      (notmuch-tree-no-match-author-face     ((t :inherit font-lock-comment-face)))
      (notmuch-tree-no-match-date-face       ((t :inherit font-lock-comment-face)))
      (notmuch-tree-match-tag-face           ((t :inherit font-lock-type-face)))
      (notmuch-tree-no-match-tag-face        ((t :inherit font-lock-type-face)))

      (company-tooltip                       ((t :inherit highlight)))
      (company-preview                       ((t :inherit font-lock-comment-face)))
      (company-preview-common                ((t :inherit font-lock-comment-face))) ;; was default
      (company-scrollbar-bg                  ((t :inherit highlight)))
      (company-scrollbar-fg                  ((t :inherit match)))
      (company-tooltip-common                ((t :inherit lazy-highlight)))
      (company-tooltip-common-selection      ((t :inherit lazy-highlight)))
      (company-tooltip-search                ((t :inherit match)))
      (company-tooltip-search-selection      ((t :inherit match)))
      (company-tooltip-selection             ((t :inherit match)))
      (company-tooltip-annotation            ((t :inherit font-lock-comment-face)))
      (company-tooltip-annotation-selection  ((t :inherit font-lock-comment-face)))

      (dired-header                          ((t :inherit font-lock-keyword-face)))
      (dired-flagged                         ((t :inherit error)))
      (dired-ignore                          ((t :inherit font-lock-comment-face)))
      (dired-mark                            ((t :inherit font-lock-string-face)))
      (dired-marked                          ((t :inherit lazy-highlight)))
      (dired-perm-write                      ((t :inherit fixed-pitch)))
      (dired-directory                       ((t :inherit fixed-pitch :weight bold)))
      (dired-symlink                         ((t :inherit link)))
      (dired-warning                         ((t :inherit font-lock-warning-face)))

      (sr-active-path-face                   ((t :inherit dired-header)))
      (sr-editing-path-face                  ((t :inherit error)))
      (sr-highlight-path-face                ((t :inherit dired-header)))
      (sr-passive-path-face                  ((t :inherit fixed-pitch)))

      (sr-marked-file-face                   ((t :inherit lazy-highlight)))
      (sr-marked-dir-face                    ((t :inherit lazy-highlight)))

      (sr-directory-face                     ((t :inherit dired-directory)))
      (sr-symlink-face                       ((t :inherit dired-symlink)))
      (sr-symlink-directory-face             ((t :inherit dired-symlink :weight bold)))
      (sr-broken-link-face                   ((t :inherit dired-warning)))

      (sr-compressed-face                    ((t :inherit fixed-pitch)))
      (sr-encrypted-face                     ((t :inherit fixed-pitch)))
      (sr-html-face                          ((t :inherit fixed-pitch)))
      (sr-log-face                           ((t :inherit fixed-pitch)))
      (sr-packaged-face                      ((t :inherit fixed-pitch)))
      (sr-xml-face                           ((t :inherit fixed-pitch)))

      (git-commit-summary                    ((t :inherit font-lock-keyword-face)))
      (git-commit-comment-heading            ((t :inherit font-lock-comment-face)))

      (compilation-message-face              ((t :inherit fixed-pitch)))
      (compilation-error                     ((t :inherit font-lock-keyword-face)))
      (compilation-line-number               ((t :inherit font-lock-builtin-face)))
      (compilation-mode-line-run             ((t :inherit font-lock-comment-face)))
      (compilation-mode-line-exit            ((t :inherit font-lock-string-face)))
      (compilation-mode-line-fail            ((t :inherit error)))

      (font-latex-sedate-face                ((t :inherit fixed-pitch)))
      (font-latex-warning-face               ((t :inherit fixed-pitch)))
      (font-latex-sectioning-0-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-1-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-2-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-3-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-4-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-5-face          ((t :inherit fixed-pitch)))
      (font-latex-sectioning-6-face          ((t :inherit fixed-pitch)))

      (ansible::task-label-face              ((t :inherit fixed-pitch)))

      )))

  ;;(message "%S" theme-faces)

  (apply #'custom-theme-set-faces 'modern-minik theme-faces)

  (custom-theme-set-variables
   'modern-minik
   '(cursor-type 'bar)

   '(org-priority-faces
     '((65 . theme-error-color)
       (66 . theme-primary-accent)
       (67 . theme-secondary-accent)
       (68 . theme-strong-diminuitive)
       (69 . theme-weak-diminuitive)
       (70 . theme-weak-diminuitive)))

   '(mode-line-format (list "%e"
                            '(:eval (when (buffer-local-value buffer-read-only
                                                              (window-buffer))
                                      "(read-only)"))
                            '(:eval (when (buffer-modified-p)
                                      "(***)"))
                            " "
                            #'mode-line-buffer-identification
                            ":"
                            '(:eval (propertize "(%l,%c)"))
                            " [%["
                            '(:eval mode-name)
                            " ("
                            '(:eval minor-mode-alist)
                            " )"
                            '(:eval (propertize "%n"))
                            "%]]"
                            #'mode-line-misc-info))))

(defvar modern-minik-mode-icon-alist
  "Alist mapping mode symbol to two strings – one unicode and one ascii.")

(setq modern-minik-mode-icon-alist
      '((auto-revert-mode (" ⭮" . " R"))
        (whitespace-mode (" ⬚" . " W"))
        (god-local-mode (" ⇪" . " G"))
        (projectile-mode (" ⎘" . " P"))
        (yas-minor-mode (" ⋯" . " ..."))
        (flycheck-mode (" ᪶" . " FC"))
        (mml-mode (" 🖃" . " M"))
        (aggressive-indent-mode (" ↹" . " AI"))
        (auto-fill-function (" ∃" . " |"))
        (isearch-mode (" 🔍" . " S"))
        (outline-minor-mode (" 𝈞" . " O"))))

(defun modern-minik-mode-icon (mode)
  "Return either unicode or ascii icon for MODE.
Chooses based on `display-graphic-p'."
  (let ((pick (if (display-graphic-p) #'car #'cdr))
        (icon (alist-get mode modern-minik-mode-icon-alist)))
    (apply pick icon)))

(defun modern-minik-set-icons ()
  "Set all mode icons in the mode-line."
  (dolist (mode (mapcar #'car modern-minik-mode-icon-alist))
    (unless (member mode '(flycheck-mode))
      (diminish mode (modern-minik-mode-icon mode)))))

(provide-theme 'modern-minik)

;;; modern-minik-theme.el ends here
