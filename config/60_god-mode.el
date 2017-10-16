(use-package god-mode :defines god-mode-isearch-map
  :bind (("<escape>" . god-local-mode)
         :map isearch-mode-map ("<escape>" . god-mode-isearch-activate)
         :map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable))

  :demand t
  
  :init
  (setq god-exempt-major-modes
        '(magit-mode
          magit-status-mode
          magit-popup-mode
          magit-diff-mode
          org-agenda-mode
          dired-mode
          notmuch-hello-mode
          notmuch-search-mode
          notmuch-show-mode
          notmuch-tree-mode))

  
  (setq god-exempt-predicates
        '(god-exempt-mode-p))

  :config
  (god-mode-all)
  (require 'god-mode-isearch))
