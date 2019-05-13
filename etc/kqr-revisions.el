;; Autorevert
(when (require 'autorevert nil)
  (global-auto-revert-mode 1))


;; Undo-tree
(autoload 'undo-tree-undo "undo-tree")
(autoload 'undo-tree-redo "undo-tree")
(autoload 'undo-tree-visualize "undo-tree")
(define-key global-map (kbd "C-/") 'undo-tree-undo)
(define-key global-map (kbd "C-?") 'undo-tree-redo)
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)
(with-eval-after-load "undo-tree"
  (diminish 'undo-tree-mode)
  (global-undo-tree-mode +1)
  (setq-default undo-tree-visualizer-diff t))


;; Merge binds
(when (require 'smerge-mode nil)
  (defun sm-try-smerge ()
    "Start smerge-mode automatically when a git conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  (add-hook 'find-file-hook 'sm-try-smerge t))


;; Ediff mode for interactive comparison of text
(when (require 'ediff nil)
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  (defun ediff-outline-show-all ()
    (when (or (eq major-mode 'org-mode)
              (eq major-mode 'outline-mode)
              outline-minor-mode)
      (outline-show-all)))
  (add-hook 'ediff-prepare-buffer-hook #'ediff-outline-show-all))
