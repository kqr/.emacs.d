(unbind-key "<f4>")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(use-package org
  :ensure org-plus-contrib
  :bind (("<f4> a" . org-agenda)
         ("<f4> c" . org-capture)
         ("<f4> o" . org-cycle-agenda-files))
  :demand t
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates

  :init
  (customize-set-variable 'org-export-backends '(html s5))
  
  :config
  (customize-set-variable
   'org-todo-keywords
   '((sequence "NEW" "HOLD" "TODO" "|" "DONE" "CANCELED")))
  
  (customize-set-variable
   'org-todo-keyword-faces
   '(("NEW" . (:foreground "red" :weight bold))
     ("TODO" . (:foreground "dark orange" :weight bold))
     ("DONE" . (:foreground "olivedrab3" :weight bold))
     ("HOLD" . (:foreground "dodger blue" :weight bold))
     ("CANCELED" . (:foreground "dim grey" :weight bold))))
  
  (customize-set-variable 'org-lowest-priority ?F)
  (customize-set-variable 'org-default-priority ?D)

  (customize-set-variable
   'org-capture-templates
   '(("n" "NEW" entry (file "") "* NEW [#D] %?\n  SCHEDULED: %t")))

  (customize-set-variable 'org-agenda-files '("~/Dropbox/Orgzly/brain.org"))
  (customize-set-variable 'org-default-notes-file "~/Dropbox/Orgzly/brain.org"))
