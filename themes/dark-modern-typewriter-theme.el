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

(defun default-with (&rest spec)
  "Define a face with SPEC that inherits from default and can be extended."
  (mapcar (lambda (props) (append props '(:inherit default))) spec))


(deftheme dark-modern-typewriter
  "A sparsely but tastefully coloured dark theme.")

(let* ((theme-strong-highlight "cornsilk3")
       (theme-medium-highlight "gray20")
       (theme-weak-highlight "gray11")
       (theme-error-color "red")
       (theme-primary-accent "chocolate2")
       (theme-secondary-accent "dodger blue")
       (theme-strong-diminuitive "olivedrab3")
       (theme-weak-diminuitive "cornsilk4")
       
       (theme-faces
        (from-faces-map
         (list
          ;; Set the default face, which is inherited by most things
          '(((t
              :background "black" :foreground "cornsilk2"
              :foundry "b&h" :family "Luxi Mono" :height 135))
            default)

          ;; Faces that inherit the pure default face and nothing else
          (cons (default-with '(t))
                '(org-level-1
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
                  whitespace-space))

          (cons (default-with `(t :weight bold))
                '(undo-tree-visualizer-active-branch-face))
          
          (cons (default-with `(t :inverse-video t))
                '(region))

          (cons (default-with `(t :background ,theme-strong-highlight
                                  :foreground "black"))
                '(mode-line))

          (cons (default-with `(t :background ,theme-medium-highlight))
                '(show-paren-match))
          
          (cons (default-with `(t :background ,theme-weak-highlight))
                '(highlight
                  widget-field))
          
          (cons (default-with `(t :foreground ,theme-error-color
                                  :weight bold))
                '(error
                  show-paren-mismatch
                  column-enforce-face
                  whitespace-trailing
                  isearch-fail
                  flycheck-fringe-error))
          
          (cons (default-with `(t :underline
                                  (:color ,theme-error-color :style wave)))
                '(flycheck-error))
          
          (cons (default-with `(t :foreground ,theme-primary-accent))
                '(warning
                  org-level-2
                  org-scheduled-previously
                  flycheck-fringe-warning
                  undo-tree-visualizer-current-face
                  notmuch-search-matching-authors
                  notmuch-search-non-matching-authors))
          
          (cons (default-with `(t :underline
                                  (:color ,theme-primary-accent :style wave)))
                '(flycheck-warning))
          
          (cons (default-with `(t :foreground ,theme-primary-accent
                                  :weight bold))
                '(minibuffer-prompt
                  font-lock-keyword-face
                  font-lock-warning-face
                  fic-face
                  fic-author-face))

          (cons (default-with `(t :foreground ,theme-primary-accent
                                  :inverse-video t))
                '(match))
          
          (cons (default-with `(t :foreground ,theme-secondary-accent))
                '(org-level-3
                  org-priority
                  org-agenda-structure
                  font-lock-type-face
                  flycheck-fringe-info
                  notmuch-tag-face
                  notmuch-tree-match-tag-face
                  notmuch-tree-no-match-tag-face))
          
          (cons (default-with `(t :underline
                                  (:color ,theme-secondary-accent :style wave)))
                '(flycheck-info))
          
          (cons (default-with `(t :foreground ,theme-secondary-accent
                                  :underline t))
                '(link
                  button
                  org-link))

          (cons (default-with `(t :foreground ,theme-secondary-accent
                                  :inverse-video t))
                '(lazy-highlight))
          
          (cons (default-with `(t :foreground ,theme-strong-diminuitive))
                '(org-level-4
                  font-lock-string-face
                  undo-tree-visualizer-unmodified-face
                  notmuch-wash-cited-text))

          (cons (default-with `(t :foreground ,theme-strong-diminuitive
                                  :inverse-video t))
                '(escape-glyph))
          
          (cons (default-with `(t :foreground ,theme-weak-diminuitive))
                '(mode-line-inactive
                  org-specissal-keyword
                  org-scheduled
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
                  notmuch-tree-no-match-date-face))))))

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
