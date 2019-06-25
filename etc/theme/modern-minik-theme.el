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

(defface theme-base-face ()
  "We need to set the background on DEFAULT to the value we want
  as our background in Emacs, but we don't want to inherit from
  DEFAULT in other faces, because then when we htmlize source
  code, each individual word may have a different background than
  what it's rendered on. By having this as an alternate default
  except with no background specified, we get the best of both
  worlds, without having to pick between fixed and variable pitch
  at theme time.")

(let* ((light? (equal 'light frame-background-mode))
       (theme-base-height (face-attribute 'fixed-pitch :height nil 'default))
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
           (((t :background ,theme-background-color
                :foreground ,theme-default-color
                :height ,theme-base-height))
            default)

           ;; Set this without the background, so we can inherit from them
           (((t :foreground ,theme-default-color))
            theme-base-face)

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
            font-lock-comment-face)

           (((t :background ,theme-weak-diminuitive
                :foreground ,theme-background-color))
            spaceline-evil-insert)

           (((t :background ,theme-primary-accent
                :foreground ,theme-background-color))
            spaceline-evil-normal)

           (((t :background ,theme-strong-diminuitive
                :foreground ,theme-background-color))
            spaceline-evil-emacs)

           (((t :background ,theme-secondary-accent
                :foreground ,theme-background-color))
            spaceline-evil-visual)

           ))))

  (setq
   theme-faces
   (append
    theme-faces
    '(
      ;; For some reason, specifying the height on fixed-pitch makes it
      ;; correctly sized under variable-pitch-mode – whereas if I don't,
      ;; it gets huge. Doesn't seem to matter what I set the size to,
      ;; though, so I'm setting it to what it's already at. Very odd!
      (fixed-pitch                           ((t :inherit theme-base-face
                                                 :height 1.0)))
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

      (powerline-active1                     ((t :inherit mode-line :background "gray11")))
      (powerline-active2                     ((t :inherit mode-line :background "gray20")))
      (powerline-inactive1                   ((t :inherit mode-line-inactive)))
      (powerline-inactive2                   ((t :inherit mode-line-inactive)))

      (avy-lead-face                         ((t :inherit lazy-highlight)))
      (avy-lead-face-0                       ((t :inherit lazy-highlight)))
      (avy-lead-face-1                       ((t :inherit match)))
      (avy-lead-face-2                       ((t :inherit lazy-highlight)))

      (ivy-current-match                     ((t :inherit match)))
      (ivy-minibuffer-match-face-1           ((t :inherit theme-base-face)))
      (ivy-minibuffer-match-face-2           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-3           ((t :inherit lazy-highlight)))
      (ivy-minibuffer-match-face-4           ((t :inherit lazy-highlight)))
      (ivy-cursor                            ((t :inherit cursor)))
      (ivy-match-required-face               ((t :inherit isearch-fail)))
      (swiper-match-face-1                   ((t :inherit lazy-highlight)))
      (swiper-match-face-2                   ((t :inherit lazy-highlight)))
      (swiper-match-face-3                   ((t :inherit lazy-highlight)))
      (swiper-match-face-4                   ((t :inherit lazy-highlight)))

      (undo-tree-visualizer-default-face     ((t :inherit (font-lock-comment-face fixed-pitch))))
      (undo-tree-visualizer-current-face     ((t :inherit (warning fixed-pitch))))
      (undo-tree-visualizer-unmodified-face  ((t :inherit (font-lock-string-face fixed-pitch))))

      (org-level-1                           ((t :inherit default
                                                 :height 1.25)))
      (org-level-2                           ((t :inherit default
                                                 :height 1.0
                                                 :weight bold)))
      (org-level-3                           ((t :inherit default
                                                 :height 1.0
                                                 :slant italic)))
      (org-level-4                           ((t :inherit default)))
      (org-level-5                           ((t :inherit default)))
      (org-level-6                           ((t :inherit default)))
      (org-level-7                           ((t :inherit default)))
      (org-level-8                           ((t :inherit default)))
      (org-level-9                           ((t :inherit default)))

      (org-indent                            ((t :inherit (org-hide fixed-pitch))))

      (org-document-title                    ((t :inherit default
                                                 :weight bold
                                                 :height 1.25)))
      (org-document-info                     ((t :inherit default
                                                 :slant italic
                                                 :height 1.0)))
      (org-meta-line                         ((t :inherit (font-lock-comment-face fixed-pitch)
                                                 :height 0.75)))
      (org-document-info-keyword             ((t :inherit org-meta-line)))

      (org-block                             ((t :inherit fixed-pitch
                                                 :height 0.75)))
      (org-block-begin-line                  ((t :inherit org-meta-line)))
      (org-block-end-line                    ((t :inherit org-meta-line)))
      (org-table                             ((t :inherit org-block)))
      (org-code                              ((t :inherit org-block)))
      (org-block-background                  ((t :inherit org-block)))
      (org-quote                             ((t :inherit defaul
                                                 :slant italic)))
      (org-link                              ((t :inherit link)))
      (org-footnote                          ((t :inherit font-lock-comment-face)))
      (org-special-keyword                   ((t :inherit font-lock-comment-face)))

      (org-checkbox                          ((t :inherit org-block)))

      (org-ellipsis                          ((t :inherit font-lock-keyword-face)))
      (org-tag                               ((t :inherit (font-lock-comment-face org-block))))
      (org-todo                              ((t :inherit (font-lock-keyword-face org-block))))
      (org-done                              ((t :inherit (font-lock-comment-face org-block))))
      (org-agenda-done                       ((t :inherit font-lock-comment-face)))
      (org-upcoming-deadline                 ((t :inherit font-lock-keyword-face)))
      (org-scheduled                         ((t :inherit font-lock-comment-face)))
      (org-scheduled-today                   ((t :inherit theme-base-face)))
      (org-scheduled-previously              ((t :inherit warning)))
      (org-priority                          ((t :inherit (font-lock-type-face org-block))))
      (org-agenda-structure                  ((t :inherit font-lock-type-face)))
      (org-agenda-date                       ((t :inherit theme-base-face)))
      (org-agenda-date-today                 ((t :inherit theme-base-face)))
      (org-agenda-date-weekend               ((t :inherit theme-base-face)))
      (org-warning                           ((t :inherit error)))

      (org-drill-hidden-cloze-face           ((t :inherit widget-field)))
      (org-drill-visible-cloze-face          ((t :inherit font-lock-string-face)))
      (org-drill-visible-cloze-hint-face     ((t :inherit font-lock-comment-face)))

      (font-lock-constant-face               ((t :inherit theme-base-face)))
      (font-lock-variable-name-face          ((t :inherit theme-base-face)))
      (font-lock-doc-face                    ((t :inherit font-lock-comment-face)))
      (font-lock-function-name-face          ((t :inherit theme-base-face :slant italic)))
      (font-lock-warning-face                ((t :inherit flycheck-warning)))
      (jdee-font-lock-public-face            ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-private-face           ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-protected-face         ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-modifier-face          ((t :inherit font-lock-keyword-face)))
      (jdee-font-lock-package-face           ((t :inherit theme-base-face)))
      (jdee-font-lock-number-face            ((t :inherit theme-base-face)))
      (jdee-font-lock-constant-face          ((t :inherit theme-base-face)))
      (jdee-font-lock-constructor-face       ((t :inherit theme-base-face)))

      (js2-function-param                    ((t :inherit theme-base-face)))
      (js2-function-call                     ((t :inherit theme-base-face)))
      (js2-external-variable                 ((t :inherit flycheck-warning)))
      (js2-warning                           ((t :inherit flycheck-warning)))
      (js2-jsdoc-tag                         ((t :inherit font-lock-keyword-face)))
      (js2-jsdoc-type                        ((t :inherit font-lock-type-face)))

      (fsharp-ui-operator-face               ((t :inherit theme-base-face)))

      (sh-heredoc                            ((t :inherit font-lock-string-face)))
      (ess-backquoted-face                   ((t :inherit theme-base-face)))

      (shm-current-face                      ((t :inherit show-paren-match)))
      (shm-quarantine-face                   ((t :inherit theme-base-face)))

      (nxml-element-local-name               ((t :inherit default)))
      (nxml-attribute-local-name             ((t :inherit default)))

      (outshine-level-1                      ((t :inherit font-lock-comment-face)))
      (outshine-level-2                      ((t :inherit font-lock-comment-face)))
      (outshine-level-3                      ((t :inherit font-lock-comment-face)))
      (outshine-level-4                      ((t :inherit font-lock-comment-face)))

      (whitespace-hspace                     ((t :inherit theme-base-face)))
      (whitespace-newline                    ((t :inherit theme-base-face)))
      (whitespace-tab                        ((t :inherit theme-base-face)))
      (whitespace-space                      ((t :inherit theme-base-face)))
      (whitespace-trailing                   ((t :inherit font-lock-keyword-face)))
      (column-enforce-face                   ((t :inherit error)))

      (ediff-current-diff-A                  ((t :background "#503050")))
      (ediff-current-diff-B                  ((t :background "#705000")))
      (ediff-current-diff-C                  ((t :background "#407010")))
      (ediff-fine-diff-A                     ((t :background "#7040a0")))
      (ediff-fine-diff-B                     ((t :background "#907000")))
      (ediff-fine-diff-C                     ((t :background "#509040")))
      (ediff-even-diff-Ancestor              ((t :inherit powerline-active1)))
      (ediff-even-diff-A                     ((t :inherit powerline-active2)))
      (ediff-even-diff-B                     ((t :inherit powerline-active1)))
      (ediff-even-diff-C                     ((t :inherit powerline-active2)))
      (ediff-odd-diff-Ancestor               ((t :inherit powerline-active2)))
      (ediff-odd-diff-A                      ((t :inherit powerline-active1)))
      (ediff-odd-diff-B                      ((t :inherit powerline-active2)))
      (ediff-odd-diff-C                      ((t :inherit powerline-active1)))

      (fic-face                              ((t :inherit error)))
      (fic-author-face                       ((t :inherit font-lock-keyword-face)))
      (flycheck-fringe-info                  ((t :inherit font-lock-string-face)))
      (flycheck-fringe-warning               ((t :inherit warning)))
      (flycheck-fringe-error                 ((t :inherit error)))

      (message-header-subject                ((t :inherit theme-base-face)))
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
      (dired-perm-write                      ((t :inherit theme-base-face)))
      (dired-directory                       ((t :inherit theme-base-face :weight bold)))
      (dired-symlink                         ((t :inherit link)))
      (dired-warning                         ((t :inherit font-lock-warning-face)))

      (sr-active-path-face                   ((t :inherit dired-header)))
      (sr-editing-path-face                  ((t :inherit error)))
      (sr-highlight-path-face                ((t :inherit dired-header)))
      (sr-passive-path-face                  ((t :inherit theme-base-face)))

      (sr-marked-file-face                   ((t :inherit lazy-highlight)))
      (sr-marked-dir-face                    ((t :inherit lazy-highlight)))

      (sr-directory-face                     ((t :inherit dired-directory)))
      (sr-symlink-face                       ((t :inherit dired-symlink)))
      (sr-symlink-directory-face             ((t :inherit dired-symlink :weight bold)))
      (sr-broken-link-face                   ((t :inherit dired-warning)))

      (sr-compressed-face                    ((t :inherit theme-base-face)))
      (sr-encrypted-face                     ((t :inherit theme-base-face)))
      (sr-html-face                          ((t :inherit theme-base-face)))
      (sr-log-face                           ((t :inherit theme-base-face)))
      (sr-packaged-face                      ((t :inherit theme-base-face)))
      (sr-xml-face                           ((t :inherit theme-base-face)))

      (git-commit-summary                    ((t :inherit font-lock-keyword-face)))
      (git-commit-comment-heading            ((t :inherit font-lock-comment-face)))

      (compilation-message-face              ((t :inherit theme-base-face)))
      (compilation-error                     ((t :inherit font-lock-keyword-face)))
      (compilation-line-number               ((t :inherit font-lock-builtin-face)))
      (compilation-mode-line-run             ((t :inherit font-lock-comment-face)))
      (compilation-mode-line-exit            ((t :inherit font-lock-string-face)))
      (compilation-mode-line-fail            ((t :inherit error)))

      (font-latex-sedate-face                ((t :inherit theme-base-face)))
      (font-latex-warning-face               ((t :inherit theme-base-face)))
      (font-latex-sectioning-0-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-1-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-2-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-3-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-4-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-5-face          ((t :inherit theme-base-face)))
      (font-latex-sectioning-6-face          ((t :inherit theme-base-face)))

      (markdown-pre-face                     ((t :inherit org-block)))
      (markdown-code-face                    ((t :inherit org-code)))
      (markdown-header-face-1                ((t :inherit org-level-1)))
      (markdown-header-face-2                ((t :inherit org-level-2)))
      (markdown-header-face-3                ((t :inherit org-level-3)))
      (markdown-header-face-4                ((t :inherit org-level-4)))
      (markdown-header-face-5                ((t :inherit org-level-5)))
      (markdown-header-face-6                ((t :inherit org-level-6)))

      (ansible::task-label-face              ((t :inherit theme-base-face))))))

  ;;(message "%S" theme-faces)

  (apply #'custom-theme-set-faces 'modern-minik theme-faces)

  (custom-theme-set-variables
   'modern-minik
   '(cursor-type 'bar)

   `(org-priority-faces
     '((65 . ,theme-error-color)
       (66 . ,theme-primary-accent)
       (67 . ,theme-secondary-accent)
       (68 . ,theme-strong-diminuitive)
       (69 . ,theme-weak-diminuitive)
       (70 . ,theme-weak-diminuitive)))

   `(org-drill-new-count-color ,theme-primary-accent)
   `(org-drill-done-count-color ,theme-weak-diminuitive)
   `(org-drill-failed-count-color ,theme-error-color)
   `(org-drill-mature-count-color ,theme-strong-diminuitive)
   '(org-drill-use-visible-cloze-face-p t)

   '(powerline-default-separator 'wave)))

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

(defun modern-minik-configure-spaceline ()
  (when (require 'spaceline-config nil)
    ;;(spaceline-reset)
    (spaceline-emacs-theme)))

(defun modern-minik-configure-org ()
  (when (require 'org nil)
    (setq org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t)))

(defun modern-minik-eval-init ()
  (modern-minik-configure-org)
  (modern-minik-set-icons)
  (modern-minik-configure-spaceline))

(provide-theme 'modern-minik)
;;; modern-minik-theme.el ends here
