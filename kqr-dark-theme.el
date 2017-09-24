;;; kqr-dark-theme --- A sparsely but tastefully coloured dark theme.
;;; Commentary:
;;; Code:

(defvar kqr-dark-theme
  "A sparsely but tastefully coloured dark theme.")

(defun apply-kqr-dark-theme ()
  "Apply the kqr-dark-theme theme."
  ;; Apply the theme, ignoring faces that don't exist
  (dolist (prop-group kqr-dark-theme)
    (let ((prop (car prop-group)) (values (cdr prop-group)))
      (dolist (value-group values)
	(let ((value (car value-group)) (faces (cdr value-group)))
	  (dolist (face faces)
	    (when (facep face)
	      (set-face-attribute face nil prop value))))))))

;;  (set-face-attribute 'highlight nil :background "grey11")

(setq kqr-dark-theme
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
		      widget-field)
		     
		     ("red"
		      show-paren-mismatch
		      trailing-whitespace
		      whitespace-hspace
		      whitespace-newline
		      whitespace-space
		      whitespace-tab))

	(:foreground ("AntiqueWhite2"
		      default)
		     
		     ("default"
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
		     
		     ;; Primary accent colour
		     ("dark orange"
		      org-level-1
		      font-lock-keyword-face
		      notmuch-search-matching-authors
		      notmch-search-non-matching-authors
		      )

		     ;; Secondary accent colour
		     ("dodger blue"
		      org-level-2
		      org-link
		      font-lock-type-face
		      notmuch-tag-face
		      notmuch-tree-match-tag-face
		      notmuch-tree-no-match-tag-face)
		     
		     ;; Visible diminuitive colour
		     ("OliveDrab3"
		      org-level-3
		      font-lock-string-face
		      notmuch-wash-cited-text)

		     ;; Receding diminuitive colour
		     ("dim grey"
		      mode-line-inactive
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
		  message-header-subject
		  org-level-1))
	
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
