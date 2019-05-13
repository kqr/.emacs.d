;;;; Emacs Speaks Statistics, used for R
(autoload 'ess-r-mode "ess-site")
(push '("\\.r\\'" . ess-r-mode) auto-mode-alist)
