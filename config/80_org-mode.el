(unbind-key "<f4>")

(add-to-list 'package-archives'("org" . "http://orgmode.org/elpa/") t)

(use-package org-plus-contrib
  :bind (("<f4> a" . org-agenda)
         ("<f4> c" . org-capture)
         ("<f4> o" . org-cycle-agenda-files)
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates

  :init
  (setq org-export-backends '(html s5))
  
  :config
  (setq org-todo-keywords '((sequence "NEW" "HOLD" "TODO" "|" "DONE" "CANCELED")))
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
