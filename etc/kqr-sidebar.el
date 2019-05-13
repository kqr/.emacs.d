(define-key global-map (kbd "<C-tab>") 'dired-sidebar-jump-to-sidebar)
(autoload 'dired-sidebar-jump-to-sidebar "dired-sidebar")
(with-eval-after-load "dired-sidebar"
  (setq dired-sidebar-subtree-line-prefix " .")
  (setq dired-sidebar-close-sidebar-on-file-open t))
(when (and
       (display-graphic-p)
       (require 'all-the-icons nil)
       (require 'all-the-icons-dired nil))
  (all-the-icons-dired-mode)
  (diminish 'all-the-icons-dired-mode))
