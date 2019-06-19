(when (require 'epresent)
  (add-to-list 'evil-emacs-state-modes 'epresent-mode) ;; only works sometimes???
  (setq epresent-text-scale 120)

  (define-advice epresent--get-frame
      (:around (actual-get-frame &rest args) epresent-frame-set-buffer)
    (let ((presentation-buffer (current-buffer)))
      (apply actual-get-frame args)
      (switch-to-buffer presentation-buffer)
      epresent--frame))

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
