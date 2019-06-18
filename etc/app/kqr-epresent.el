(when (require 'epresent)
  (add-to-list 'evil-emacs-state-modes 'epresent-mode) ;; only works sometimes???
  (setq epresent-text-scale 120)

  (define-advice epresent--get-frame
      (:around (actual-get-frame &rest args) epresent-frame-set-buffer)
    (let* ((presentation-buffer (current-buffer))
           (presentation-frame (apply actual-get-frame args)))
      (message "presentation buffer: %S" presentation-buffer)
      (message "presentation frame: %S" presentation-frame)
      (message "epresent-frame: %S" epresent--frame)
      (with-selected-frame epresent--frame
        (switch-to-buffer presentation-buffer))
      epresent--frame)))

;; Bad place for this but I can't be arsed to come up with a better one
;; (defun remove-all-advice-for (symbol)
;;   (interactive "a")
;;   (let ((removed-advices 0))
;;     (advice-mapc
;;      (lambda (advice props)
;;        (when advice
;;          (incf removed-advices)
;;          (advice-remove symbol advice)))
;;      symbol)
;;     (message "Removed %d pieces of advice" removed-advices)))
