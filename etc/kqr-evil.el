(when (require 'evil nil)
  (when (require 'tab-as-escape nil)
    (diminish 'tab-as-escape-mode)
    (tab-as-escape-mode +1))
  ;; Don't override tab as outline mode subtree cycle
  (define-key evil-motion-state-map (kbd "TAB") nil)

  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (define-key evil-normal-state-map (kbd ";") #'evil-ex)

  (define-advice evil-quit
      (:before-until (&rest args) evil-quit-kills-buffer-first)
    (kill-buffer))

  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                (outline-previous-visible-heading 1))))

  (setq evil-want-fine-undo t
        evil-move-beyond-eol t
        evil-move-cursor-back nil)

  (with-eval-after-load "ace-window"
    (define-key evil-normal-state-map (kbd "M-o") 'ace-window)
    (define-key evil-insert-state-map (kbd "M-o") 'ace-window)
    (define-key evil-visual-state-map (kbd "M-o") 'ace-window))

  (when (require 'spaceline-config nil)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

  (when (require 'evil-cleverparens nil)
    (add-hook 'smartparens-enabled-hook 'evil-cleverparens-mode))

  (when (and (require 'vimish-fold nil)
             (require 'evil-vimish-fold nil))
    (setq vimish-fold-header-width 70)
    (evil-vimish-fold-mode +1))

  (let ((leader-key-map (make-sparse-keymap)))
    (when (require 'evil-commentary nil)
      (evil-commentary-mode))
    (when (require 'avy nil)
      (setq avy-keys'(?a ?r ?s ?t ?n ?e ?i ?o)
            avy-background t
            avy-all-windows t)
      (define-key leader-key-map (kbd "w") 'avy-goto-word-1)
      (define-key leader-key-map (kbd "k") 'avy-goto-line))
    (when (require 'evil-surround nil)
      (global-evil-surround-mode 1))
    (when (require 'evil-visualstar nil)
      (global-evil-visualstar-mode 1))

    (define-key evil-normal-state-map (kbd "") nil)
    (define-key evil-normal-state-map (kbd "SPC") leader-key-map))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (evil-mode 1))
