;; Emacs Lisp and CL stuff
(defun configure-lisp-mode ()
  (setq c-basic-offset 2))

(add-hook 'emacs-lisp-mode-hook 'configure-lisp-mode)
(add-hook 'lisp-mode-hook 'configure-lisp-mode)

(autoload 'slime "slime")
(autoload 'slime-connected-p "slime")

(defun start-slime ()
  "If slime is not running, start it if possible."
  (unless (slime-connected-p)
    (condition-case-unless-debug nil
        (save-excursion (slime))
      (error (message "Slime failed to start. This may be expected.")))))

(add-hook 'lisp-mode-hook 'start-slime)

(with-eval-after-load "slime"
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-asdf))
  (defun popup-slime-documentation (symbol-name)
    "Popup function- or symbol-documentation for SYMBOL-NAME."
    (interactive (list (slime-read-symbol-name "Documentation for symbol: ")))
    (when (not symbol-name)
      (error "No symbol given"))
    (slime-eval-async `(swank:documentation-symbol ,symbol-name) 'popup-tip)))
