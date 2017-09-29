;;; kqr-dark-theme --- A sparsely but tastefully coloured dark theme.
;;; Commentary:
;;; Code:

(defvar kqr-dark-faces
  "A sparsely but tastefully coloured dark theme.")

(defun apply-kqr-dark-theme ()
  "Apply the kqr-dark-theme theme."
  
  (setq-default org-priority-faces
		'((65 . "red")
		  (66 . "dark orange")
		  (67 . "dodger blue")
		  (68 . "OliveDrab3")
		  (69 . "dim gray")
		  (70 . "dim gray")))

  ;; Apply the faces, ignoring faces that don't exist
  (dolist (prop-group kqr-dark-faces)
    (let ((prop (car prop-group)) (values (cdr prop-group)))
      (dolist (value-group values)
	(let ((value (car value-group)) (faces (cdr value-group)))
	  (dolist (face faces)
	    (when (facep face)
	      (set-face-attribute face nil prop value))))))))



(setq kqr-dark-faces
      '((:background ("black"
		      default
		      fringe
		      mode-line-inactive)

		     ("AntiqueWhite2"
		      region)
		     
		     ("OliveDrab"
		      mode-line)

		     ("gray20"
		      show-paren-match)

		     ("gray11"
		      highlight
		      widget-field)
		     
		     ("red"
		      show-paren-mismatch
		      trailing-whitespace
		      whitespace-hspace
		      whitespace-newline
		      whitespace-space
		      whitespace-tab)

		     ("default"
		      fic-face))

	(:foreground ("AntiqueWhite2"
		      default)
		     
		     ("default"
		      org-level-1
		      org-scheduled-today
		      font-lock-builtin-face
		      font-lock-constant-face
		      font-lock-variable-name-face
		      font-lock-function-name-face
		      message-header-subject
		      widget-field)

		     ;; For inverted faces
		     ("black"
		      mode-line
		      region
		      trailing-whitespace
		      whitespace-hspace
		      whitespace-newline
		      whitespace-space
		      whitespace-tab)
		     
		     ;; Strong highlight, to be used sparingly
		     ("red"
		      fic-face)

		     ;; Primary accent colour
		     ("dark orange"
		      org-level-2
		      org-scheduled-previously
		      font-lock-keyword-face
		      notmuch-search-matching-authors
		      notmch-search-non-matching-authors
		      )

		     ;; Secondary accent colour
		     ("dodger blue"
		      org-level-3
		      org-priority
		      org-link
		      org-agenda-structure
		      font-lock-type-face
		      notmuch-tag-face
		      notmuch-tree-match-tag-face
		      notmuch-tree-no-match-tag-face)
		     
		     ;; Visible diminuitive colour
		     ("OliveDrab3"
		      org-level-4
		      org-agenda-done
		      font-lock-string-face
		      notmuch-wash-cited-text)

		     ;; Receding diminuitive colour
		     ("dim grey"
		      mode-line-inactive
		      org-special-keyword
		      org-agenda-date
		      org-agenda-date-today
		      org-agenda-date-weekend
		      font-lock-comment-face
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
		      notmuch-tree-no-match-date-face))

	(:weight (bold
		  fic-face
		  org-level-1
		  message-header-subject))
	
	(:foundry ("b&h"
		   default))

	(:family ("Luxi Mono"
		  default))

	(:height (135
		  default))

	(:box (nil
	       mode-line
	       mode-line-inactive))))

;;; kqr-dark-theme.el ends here
