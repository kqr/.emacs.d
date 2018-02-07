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
       (theme-strong-highlight (if light? "antiquewhite4" "antiquewhite3"))
       (theme-medium-highlight (if light? "antiquewhite3" "gray20"))
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
           (((t
              :background ,theme-background-color
              :foreground ,theme-default-color))
            default
            variable-pitch
            fixed-pitch)
           
           ;; Faces that necessarily use fixed pitch, even with variable default
           (((t :inherit fixed-pitch))
            org-table
            org-code
            org-block
            org-block-background)

           ;; Where we need to override silly defaults...
           (((t :inherit unspecified))
            ess-backquoted-face)
           
           ;; Faces that inherit the pure default face and nothing else
           (((t :foreground ,theme-default-color))
            org-level-1
            org-level-2
            org-level-3
            org-level-4
            org-level-5
            org-level-6
            org-level-7
            org-level-8
            org-level-9
            org-scheduled-today
            org-agenda-date
            org-agenda-date-today
            org-agenda-date-weekend
            font-lock-builtin-face
            font-lock-constant-face
            font-lock-variable-name-face
            font-lock-function-name-face
            message-header-subject
            whitespace-hspace
            whitespace-newline
            whitespace-tab
            whitespace-space)

           (((t :weight bold))
            undo-tree-visualizer-active-branch-face)

           (((t :inverse-video t))
            region)

           (((t :background ,theme-strong-highlight
                :foreground ,theme-background-color))
            mode-line)

           (((t :background ,theme-medium-highlight))
            show-paren-match
            widget-field
            secondary-selection)

           (((t :background ,theme-weak-highlight))
            fringe
            highlight)

           (((t :foreground ,theme-error-color
                :weight bold))
            error
            show-paren-mismatch
            column-enforce-face
            whitespace-trailing
            isearch-fail
            flycheck-fringe-error
            fic-face)

           (((t :underline
                (:color ,theme-error-color :style wave)))
            flycheck-error)

           (((t :foreground ,theme-primary-accent))
            warning
            org-scheduled-previously
            flycheck-fringe-warning
            undo-tree-visualizer-current-face
            notmuch-search-matching-authors
            notmuch-search-non-matching-authors)

           (((t :underline
                (:color ,theme-primary-accent :style wave)))
            font-lock-warning-face
            flycheck-warning)

           (((t :foreground ,theme-primary-accent
                :weight bold))
            minibuffer-prompt
            font-lock-keyword-face
            org-todo
            fic-author-face)

           (((t :foreground ,theme-primary-accent
                :inverse-video t))
            match)

           (((t :foreground ,theme-secondary-accent))
            org-priority
            org-agenda-structure
            font-lock-type-face
            flycheck-fringe-info
            notmuch-tag-face
            notmuch-tree-match-tag-face
            notmuch-tree-no-match-tag-face)

           (((t :underline
                (:color ,theme-secondary-accent :style wave)))
            flycheck-info)

           (((t :foreground ,theme-secondary-accent
                :underline t))
            link
            button
            org-link)

           (((t :foreground ,theme-secondary-accent
                :inverse-video t))
            lazy-highlight)

           (((t :foreground ,theme-strong-diminuitive))
            font-lock-string-face
            undo-tree-visualizer-unmodified-face
            org-footnote
            notmuch-wash-cited-text
            message-cited-text)

           (((t :foreground ,theme-strong-diminuitive
                :inverse-video t))
            escape-glyph)

           (((t :foreground ,theme-weak-diminuitive))
            mode-line-inactive
            org-special-keyword
            org-scheduled
            org-done
            org-agenda-done
            font-lock-comment-face
            outshine-level-1
            outshine-level-2
            outshine-level-3
            outshine-level-4
            undo-tree-visualizer-default-face
            message-header-cc
            message-header-name
            message-header-other
            message-header-xheader
            message-separator
            message-header-to
            message-mml
            notmuch-message-summary-face
            notmuch-search-count
            notmuch-search-date
            notmuch-tree-match-author-face
            notmuch-tree-match-date-face
            notmuch-tree-no-match-author-face
            notmuch-tree-no-match-date-face)))))

  (apply #'custom-theme-set-faces
         'modern-minik
         (append
          theme-faces
          '((isearch ((t :inherit match)))
            (query-replace ((t :inherit match)))
            (ivy-current-match ((t :inherit match)))
            (ivy-minibuffer-match-face-1 ((t :inherit default)))
            (ivy-minibuffer-match-face-2 ((t :inherit lazy-highlight)))
            (ivy-minibuffer-match-face-3 ((t :inherit lazy-highlight)))
            (ivy-minibuffer-match-face-4 ((t :inherit lazy-highlight)))
            (ivy-cursor ((t :inherit cursor)))
            (ivy-match-required-face ((t :inherit isearch-fail)))
            (company-tooltip ((t :inherit fringe)))
            (company-preview ((t :inherit lazy-highlight)))
            (company-preview-common ((t :inherit match)))
            (company-scrollbar-bg ((t :inherit fringe)))
            (company-scrollbar-fg ((t :inherit match)))
            (company-tooltip-common ((t :inherit lazy-highlight)))
            (company-tooltip-common-selection ((t :inherit lazy-highlight)))
            (company-tooltip-mouse ((t :inherit match)))
            (company-tooltip-search ((t :inherit match)))
            (company-tooltip-search-selection ((t :inherit match)))
            (company-tooltip-selection ((t :inherit match))))))
  
  (custom-theme-set-variables
   'modern-minik
   '(cursor-type 'bar)
   
   '(org-priority-faces '((65 . theme-error-color)
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
                            ;; temporarily disable to see if it decreases lag
                            ;;'(:eval (propertize "(%l,%c)"))
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

(provide-theme 'modern-minik)

;;; modern-minik-theme.el ends here
