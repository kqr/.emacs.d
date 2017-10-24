;;; dark-modern-typewriter --- A sparsely but tastefully coloured dark theme.
;;; Commentary:
;;;
;;; I've used a syntax highlighting theme similar to this since the beginning of
;;; time.  The primary motivating underlying idea is that syntax highlighting
;;; *isn't actually that great*.  It may help with discerning the code
;;; structure, but beyond that it is simply in the way and distracting you from
;;; reading the code.
;;;
;;; At the start, only comments and strings had their own colour, because they
;;; can be very irrelevant to the logic and just prevent you from seeing what's
;;; important.  After a while, I decided to give keywords and types their own
;;; colour as well, because that helps with showing you the code structure.
;;;
;;; Other than that, important text should use the default face, and less
;;; important text should use a darker gray to disappear into the shadows.
;;;
;;; Code:


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

(deftheme dark-modern-typewriter
  "A sparsely but tastefully coloured dark theme.")

(let* ((theme-default-color "antiquewhite2")
       (theme-strong-highlight "antiquewhite3")
       (theme-medium-highlight "gray20")
       (theme-weak-highlight "gray11")
       (theme-error-color "red")
       (theme-primary-accent "sienna3")
       (theme-secondary-accent "dodgerblue")
       (theme-strong-diminuitive "palegreen3")
       (theme-weak-diminuitive "peachpuff4")
       
       (theme-faces
        (from-faces-map
         `(
           ;; Set the default face, which is inherited by most things
           (((t :background "black" :foreground ,theme-default-color
                :foundry "b&h" :family "Luxi Mono" :height 135))
            default)

           ;; Faces that inherit the pure default face and nothing else
           (((t :foreground ,theme-default-color))
            org-level-1
            org-level-2
            org-level-3
            org-level-4
            org-scheduled-today
            org-agenda-date
            org-agenda-date-today
            org-agenda-date-weekend
            font-lock-builtin-face
            font-lock-constant-face
            font-lock-variable-name-face
            font-lock-function-name-face
            message-header-subject
            fringe
            whitespace-hspace
            whitespace-newline
            whitespace-tab
            whitespace-space)

           (((t :weight bold))
            undo-tree-visualizer-active-branch-face)
           
           (((t :inverse-video t))
            region)

           (((t :background ,theme-strong-highlight
                :foreground "black"))
            mode-line)

           (((t :background ,theme-medium-highlight))
            show-paren-match)
           
           (((t :background ,theme-weak-highlight))
            highlight
            widget-field)
           
           (((t :foreground ,theme-error-color
                :weight bold))
            error
            show-paren-mismatch
            column-enforce-face
            whitespace-trailing
            isearch-fail
            flycheck-fringe-error)
           
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
            fic-face
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
            notmuch-wash-cited-text)

           (((t :foreground ,theme-strong-diminuitive
                :inverse-video t))
            escape-glyph)
           
           (((t :foreground ,theme-weak-diminuitive))
            mode-line-inactive
            org-specissal-keyword
            org-scheduled
            org-done
            org-agenda-done
            font-lock-comment-face
            undo-tree-visualizer-default-face
            message-header-cc
            message-header-name
            message-header-other
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
         'dark-modern-typewriter
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
            (ivy-match-required-face ((t :inherit isearch-fail))))))

  (custom-theme-set-variables
   'dark-modern-typewriter
   '(cursor-type 'bar)
   '(org-priority-faces '((65 . theme-error-color)
                          (66 . theme-primary-accent)
                          (67 . theme-secondary-accent)
                          (68 . theme-strong-diminuitive)
                          (69 . theme-weak-diminuitive)
                          (70 . theme-weak-diminuitive)))))

(provide-theme 'dark-modern-typewriter)
;;; dark-modern-typewriter-theme.el ends here
