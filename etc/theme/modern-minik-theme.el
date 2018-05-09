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
       (theme-default-color (if light? "black" "antiquewhite2"))
       (theme-background-color (if light? "#fbf1e4" "black"))
       (theme-strong-highlight "antiquewhite3")
       (theme-weak-highlight (if light? "antiquewhite2" "gray11"))
       (theme-error-color "red")
       (theme-primary-accent (if light? "chocolate3" "sienna3"))
       (theme-secondary-accent (if light? "dodgerblue3" "dodgerblue"))
       (theme-strong-diminuitive (if light? "forest green" "palegreen3"))
       (theme-weak-diminuitive "peachpuff4")

       (theme-faces
        (from-faces-map
         `(
           ;; Set the default face, which is inherited by most things
           (((t :background ,theme-background-color :foreground ,theme-default-color))
            default
            variable-pitch
            fixed-pitch)
           
           ;; Where we need to override silly defaults...
           (((t :inherit unspecified))
            ess-backquoted-face)

           ;; These are odd ones, otherwise default+bold is rarely used
           (((t :weight bold))
            undo-tree-visualizer-active-branch-face)

           ;; Region defines our "inverse video" style
           (((t :inverse-video t))
            region)

           ;; The mode-line is a lighter type of inverse video
           (((t :foreground ,theme-weak-diminuitive :background ,theme-weak-highlight))
            mode-line)
           
           (((t :background ,theme-weak-highlight))
            show-paren-match
            highlight)

           (((t :foreground ,theme-error-color :weight bold))
            error)

           (((t :underline (:color ,theme-error-color :style wave)))
            flycheck-error)

           (((t :foreground ,theme-primary-accent))
            warning)

           (((t :underline (:color ,theme-primary-accent :style wave)))
            flycheck-warning)

           (((t :foreground ,theme-primary-accent :weight bold))
            font-lock-keyword-face)

           (((t :foreground ,theme-primary-accent :inverse-video t))
            match)

           (((t :foreground ,theme-secondary-accent))
            font-lock-type-face)

           (((t :box (:line-width 1 :color ,theme-weak-diminuitive :style pressed-button)))
            org-code)
           
           (((t :underline (:color ,theme-secondary-accent :style wave)))
            flycheck-info)

           (((t :foreground ,theme-secondary-accent :underline t))
            link)

           (((t :foreground ,theme-secondary-accent :inverse-video t))
            lazy-highlight)

           (((t :foreground ,theme-strong-diminuitive))
            font-lock-string-face)

           (((t :foreground ,theme-strong-diminuitive :inverse-video t))
            escape-glyph)

           (((t :foreground ,theme-weak-diminuitive))
            font-lock-comment-face)))))

  (setq
   theme-faces
   (append
    theme-faces
    '((isearch                               ((t :inherit match)))
      (query-replace                         ((t :inherit match)))
      (isearch-fail                          ((t :inherit error)))
      (show-paren-mismatch                   ((t :inherit error)))
      
      (fringe                                ((t :inherit highlight)))
      (mode-line-inactive                    ((t :inherit font-lock-comment-face)))
      (secondary-selection                   ((t :inherit mode-line)))
      
      (widget-field                          ((t :inherit mode-line)))
      (button                                ((t :inherit link)))

      (minibuffer-prompt                     ((t :inherit font-lock-keyword-face)))

      (ivy-current-match                     ((t :inherit match)))
      (ivy-minibuffer-match-face-1           ((t :inherit default)))
      (ivy-minibuffer-match-face-2           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-3           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-4           ((t :inherit lazy-highlight)))
      (ivy-cursor                            ((t :inherit cursor)))
      (ivy-match-required-face               ((t :inherit isearch-fail)))

      (undo-tree-visualizer-default-face     ((t :inherit font-lock-comment-face)))
      (undo-tree-visualizer-current-face     ((t :inherit warning)))
      (undo-tree-visualizer-unmodified-face  ((t :inherit font-lock-string-face)))
      
      (org-level-1                           ((t :inherit default
                                                 :height 1.2
                                                 :weight bold)))
      (org-level-2                           ((t :inherit default
                                                 :height 1.1
                                                 :slant italic)))
      (org-level-3                           ((t :inherit default)))
      (org-level-4                           ((t :inherit default)))
      (org-level-5                           ((t :inherit default)))
      (org-level-6                           ((t :inherit default)))
      (org-level-7                           ((t :inherit default)))
      (org-level-8                           ((t :inherit default)))
      (org-level-9                           ((t :inherit default)))
      (org-table                             ((t :inherit fixed-pitch)))
      (org-code                              ((t :inherit fixed-pitch)))
      (org-block                             ((t :inherit fixed-pitch)))
      (org-block-background                  ((t :inherit fixed-pitch)))
      (org-link                              ((t :inherit link)))
      (org-footnote                          ((t :inherit font-lock-string-face)))
      (org-special-keyword                   ((t :inherit font-lock-comment-face)))

      (org-todo                              ((t :inherit font-lock-keyword-face)))
      (org-done                              ((t :inherit font-lock-comment-face)))
      (org-agenda-done                       ((t :inherit font-lock-comment-face)))
      (org-upcoming-deadline                 ((t :inherit font-lock-keyword-face)))
      (org-scheduled                         ((t :inherit font-lock-comment-face)))
      (org-scheduled-today                   ((t :inherit default)))
      (org-scheduled-previously              ((t :inherit warning)))
      (org-priority                          ((t :inherit font-lock-type-face)))
      (org-agenda-structure                  ((t :inherit font-lock-type-face)))
      (org-agenda-date                       ((t :inherit default)))
      (org-agenda-date-today                 ((t :inherit default)))
      (org-agenda-date-weekend               ((t :inherit default)))
      (org-warning                           ((t :inherit error)))
      
      (font-lock-builtin-face                ((t :inherit default)))
      (font-lock-constant-face               ((t :inherit default)))
      (font-lock-variable-name-face          ((t :inherit default)))
      (font-lock-function-name-face          ((t :inherit default)))
      (font-lock-warning-face                ((t :inherit flycheck-warning)))
      (jdee-font-lock-public-face            ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-private-face           ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-protected-face         ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-modifier-face          ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-package-face           ((t :inherit default)))
      (jdee-font-lock-number-face            ((t :inherit default)))
      (jdee-font-lock-constant-face          ((t :inherit default)))
      (jdee-font-lock-constructor-face       ((t :inherit default)))
      (sh-heredoc                            ((t :inherit font-lock-string-face)))

      (outshine-level-1                      ((t :inherit font-lock-comment-face)))
      (outshine-level-2                      ((t :inherit font-lock-comment-face)))
      (outshine-level-3                      ((t :inherit font-lock-comment-face)))
      (outshine-level-4                      ((t :inherit font-lock-comment-face)))
      
      (whitespace-hspace                     ((t :inherit default)))
      (whitespace-newline                    ((t :inherit default)))
      (whitespace-tab                        ((t :inherit default)))
      (whitespace-space                      ((t :inherit default)))
      (whitespace-trailing                   ((t :inherit font-lock-keyword-face)))
      (column-enforce-face                   ((t :inherit error)))

      (fic-face                              ((t :inherit error)))
      (fic-author-face                       ((t :inherit font-lock-keyword-face)))
      (flycheck-fringe-info                  ((t :inherit font-lock-string-face)))
      (flycheck-fringe-warning               ((t :inherit warning)))
      (flycheck-fringe-error                 ((t :inherit error)))
      
      (message-header-subject                ((t :inherit default)))
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
      
      (company-tooltip                       ((t :inherit fringe)))
      (company-preview                       ((t :inherit font-lock-comment-face)))
      (company-preview-common                ((t :inherit default)))
      (company-scrollbar-bg                  ((t :inherit fringe)))
      (company-scrollbar-fg                  ((t :inherit match)))
      (company-tooltip-common                ((t :inherit lazy-highlight)))
      (company-tooltip-common-selection      ((t :inherit lazy-highlight)))
      (company-tooltip-search                ((t :inherit match)))
      (company-tooltip-search-selection      ((t :inherit match)))
      (company-tooltip-selection             ((t :inherit match)))
      
      (dired-header                          ((t :inherit font-lock-keyword-face)))
      (dired-flagged                         ((t :inherit error)))
      (dired-ignore                          ((t :inherit font-lock-comment-face)))
      (dired-mark                            ((t :inherit font-lock-string-face)))
      (dired-marked                          ((t :inherit lazy-highlight)))
      (dired-perm-write                      ((t :inherit default)))
      (dired-directory                       ((t :inherit default :weight bold)))
      (dired-symlink                         ((t :inherit link)))
      (dired-warning                         ((t :inherit font-lock-warning-face)))
      
      (sr-active-path-face                   ((t :inherit dired-header)))
      (sr-editing-path-face                  ((t :inherit error)))
      (sr-highlight-path-face                ((t :inherit dired-header)))
      (sr-passive-path-face                  ((t :inherit default)))

      (sr-marked-file-face                   ((t :inherit lazy-highlight)))
      (sr-marked-dir-face                    ((t :inherit lazy-highlight)))
      
      (sr-directory-face                     ((t :inherit dired-directory)))
      (sr-symlink-face                       ((t :inherit dired-symlink)))
      (sr-symlink-directory-face             ((t :inherit dired-symlink :weight bold)))
      (sr-broken-link-face                   ((t :inherit dired-warning)))
      
      (sr-compressed-face                    ((t :inherit default)))
      (sr-encrypted-face                     ((t :inherit default)))
      (sr-html-face                          ((t :inherit default)))
      (sr-log-face                           ((t :inherit default)))
      (sr-packaged-face                      ((t :inherit default)))
      (sr-xml-face                           ((t :inherit default))))))
  
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
