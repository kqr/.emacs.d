(autoload 'c-mode "cc-mode")
(push '("\\.c\\'" . c-mode) auto-mode-alist)
(push '("\\.h\\'" . c-mode) auto-mode-alist)
(with-eval-after-load "cc-mode"
  (setq-default c-default-style "stroustrup"
                c-basic-offset 4))
