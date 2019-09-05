;; Autorevert
(when (require 'autorevert)
  (global-auto-revert-mode +1))


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
        ediff-window-setup-function 'ediff-setup-windows-plain
        ;; Documentation says to add new options after the default ones, so
        ;; that's what I'll do, even this seems to default to empty string.
        ediff-diff-options (concat ediff-diff-options "-w"))

  ;; Use git diff with the histogram algorithm if available
  (let ((git (executable-find "git")))
    (when git
      (setq ediff-diff-program (concat git)
            ediff-diff-options (concat "diff " ediff-diff-options " --histogram"))))


  (defun ediff-outline-show-all ()
    (when (or (eq major-mode 'org-mode)
              (eq major-mode 'outline-mode)
              outline-minor-mode)
      (outline-show-all)))
  (add-hook 'ediff-prepare-buffer-hook #'ediff-outline-show-all)

  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (add-hook
   'ediff-keymap-setup-hook
   (lambda () (define-key ediff-mode-map "J" 'ediff-copy-both-to-C))))
