(autoload 'ada-mode "ada-mode")
(push '("\\.adb\\'" . ada-mode) auto-mode-alist)
(push '("\\.ads\\'" . ada-mode) auto-mode-alist)
(with-eval-after-load "ada-mode"
  (setq-default flycheck-gnat-args "-gnat12")
  (setq ada-language-version 'ada2012)
  (setq ada-skel-initial-string nil))
