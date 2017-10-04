(use-package fic-mode :config
  (fic-mode +1)
  (customize-set-variable
   'fic-highlighted-words (split-string "FIXME TODO BUG XXX")))
