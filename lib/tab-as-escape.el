;;; tab-as-escape.el --- summary
;;; Commentary:
;;
;;; Code:

(defun decode-tab-into-escape (&optional frame)
  "Set the decode map of (optional) FRAME to interpret tab as escape."
  (when frame (select-frame frame))
  (define-key input-decode-map (kbd "<tab>") (kbd "<escape>"))
  (define-key input-decode-map (kbd "C-i") (kbd "<escape>"))
  (define-key input-decode-map "\e[Z" [backtab]))

(define-minor-mode tab-as-escape-mode
  "Toggle swappage of the tab and escape keys."
  :global t
  :init-value nil
  :lighter "|<-esc->|"

  ;; Translate various ways of pressing shift-tab to a regular tab
  ;; The translation map operates globally across all emacs clients,
  ;; so we only need to do this setup once
  (define-key key-translation-map (kbd "<backtab>") (kbd "TAB"))
  (define-key key-translation-map (kbd "<S-tab>") (kbd "TAB"))
  (define-key key-translation-map (kbd "<S-iso-lefttab>") (kbd "TAB"))

  ;; Then we need to set up the decode map once globally, and then again
  ;; for each client as it connects
  (decode-tab-into-escape)
  (add-hook 'server-visit-hook #'decode-tab-into-escape)
  (add-hook 'tty-setup-hook #'decode-tab-into-escape)
  (add-hook 'window-setup-hook #'decode-tab-into-escape)
  (add-to-list 'after-make-frame-functions #'decode-tab-into-escape))

(provide 'tab-as-escape)
;;; tab-as-escape.el ends here
