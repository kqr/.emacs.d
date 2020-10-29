;; XXX: Use-packageify this.
(use-package evil
  :config
  (when (require 'tab-as-escape nil)
    (diminish 'tab-as-escape-mode)
    (tab-as-escape-mode +1))

  ;; Allow escape to exit out of most things.
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; Too good to pass up on. Remove one key from all ex commands!
  (define-key evil-normal-state-map (kbd ";") #'evil-ex)

  (evil-set-leader '(normal visual) " ")

  ;; Various useful defaults.
  (setq evil-want-Y-yank-to-eol t
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-want-fine-undo nil)

  ;; evil-cleverparens takes over the good ol' <> binds in a way that's probably
  ;; good only I haven't learned to use it yet. However, the default <> binds
  ;; are also useful, and since they are available by C-d and C-t in insert
  ;; mode, why not enable them also in visual mode? I don't need to pop tags
  ;; or scroll down in visual mode very often anyway! (famous last words...)
  (evil-define-key '(insert visual) global-map
    [?\C-d] 'evil-shift-left
    [?\C-t] 'evil-shift-right)

  (define-advice evil-shift-right
      (:before (&rest args) set-evil-shift-width-to-c-basic-offset)
    (setq evil-shift-width c-basic-offset))

  (define-advice evil-shift-right
      (:after (&rest args) leave-mark-active-after-evil-shift-right)
    (setq deactivate-mark nil))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)

  ;; Don't override tab for subtree cycling in outline-mode.
  (define-key evil-motion-state-map (kbd "TAB") nil)

  ;; Convenient quit shortcuts.
  (evil-define-key 'motion evil-list-view-mode-map (kbd "q") 'quit-window)
  (define-advice evil-quit
      (:before-until (&rest args) evil-quit-kills-buffer-first)
    (kill-buffer))

  (evil-mode 1))

(use-package spaceline
  :after (evil)
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package evil-cleverparens
  :after (evil)
  :hook (smartparens-enabled . evil-cleverparens-mode))

(use-package vimish-fold
  :config
  (setq vimish-fold-header-width 70))

(use-package evil-vimish-fold
  :after (evil vimish-fold)
  :config
  (evil-vimish-fold-mode +1))

(use-package evil-commentary
  :config
  (evil-commentary-mode +1))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

;; XXX: Fix these
;; (let ((leader-key-map (make-sparse-keymap)))
;;   (when (require 'avy nil)
;;     (setq avy-keys'(?a ?r ?s ?t ?n ?e ?i ?o)
;;           avy-background t
;;           avy-all-windows t)
;;     (define-key leader-key-map (kbd "w") 'avy-goto-word-1)
;;     (define-key leader-key-map (kbd "k") 'avy-goto-line))
;;   (when (require 'evil-visualstar nil)
;;     (global-evil-visualstar-mode 1))

;;   (define-key evil-normal-state-map (kbd "") nil)
;;   (define-key evil-normal-state-map (kbd "SPC") leader-key-map))
