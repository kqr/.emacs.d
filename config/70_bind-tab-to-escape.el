(defun setup-tab-decode (&optional frame)
  "Sets the decode map to interpret tab as escape."
  (when frame
    (select-frame frame))
  (define-key input-decode-map (kbd "<tab>") (kbd "<escape>"))
  (define-key input-decode-map (kbd "C-i") (kbd "<escape>"))
  (define-key input-decode-map "\e[Z" [backtab]))

;; Translation maps work "globally" across all emacs clients
(define-key key-translation-map (kbd "<backtab>") (kbd "TAB"))
(define-key key-translation-map (kbd "<S-iso-lefttab>") (kbd "TAB"))

;; Decode maps need to be set up once per frame
(setup-tab-decode)
(add-hook 'server-visit-hook #'setup-tab-decode)
(add-hook 'tty-setup-hook #'setup-tab-decode)
(add-hook 'window-setup-hook #'setup-tab-decode)
(add-hook 'after-make-frame-functions #'setup-tab-decode)
(add-to-list 'after-make-frame-functions #'setup-tab-decode)
