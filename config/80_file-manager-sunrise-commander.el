(unbind-key "<f2>")

(add-to-list 'package-archives
             '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))

(use-package sunrise-commander :bind
  (("<f2>" . sunrise-cd))

  :config
  (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode)))
