(use-package org
  :bind (("C-c C-a" . org-agenda)
         ("C-c C-c" . org-capture)
         ("C-c C-o" . org-cycle-agenda-files)
         ("M-g" . org-agenda-redo))  ;; More convenient in God mode
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates
  :config
  (setq org-todo-keywords '((sequence "NEW" "TODO" "|" "DONE" "HOLD" "CANCELED")))
  (setq org-todo-keyword-faces
        '(("NEW" . (:foreground "red" :weight bold))
          ("TODO" . (:foreground "dark orange" :weight bold))
          ("DONE" . (:foreground "olivedrab3" :weight bold))
          ("HOLD" . (:foreground "dodger blue" :weight bold))
          ("CANCELED" . (:foreground "dim grey" :weight bold))))
  
  (setq org-lowest-priority ?F)
  (setq org-default-priority ?D)

  (setq org-capture-templates
        '(("n" "NEW" entry (file "") "* NEW [#D] %?\n  SCHEDULED: %t")))
  
  (setq org-agenda-files '("~/Dropbox/Orgzly/brain.org"))
  (setq org-default-notes-file "~/Dropbox/Orgzly/brain.org"))
