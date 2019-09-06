(when (require 'epresent)
  (add-to-list 'evil-emacs-state-modes 'epresent-mode) ;; only works sometimes???

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

;; Bad place for this but I can't be arsed to come up with a better one
(defun remove-all-advice-for (symbol)
  (interactive "a")
  (let ((removed-advices 0))
    (advice-mapc
     (lambda (advice props)
       (when advice
         (incf removed-advices)
         (advice-remove symbol advice)))
     symbol)
    (message "Removed %d pieces of advice" removed-advices)))
