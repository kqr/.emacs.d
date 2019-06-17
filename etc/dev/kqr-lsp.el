;;; kqr-lsp.el --- Let's try LSP mode
;;; Commentary:
;;
;; This is currently only enabled for some modes in order to make
;; performance and/or stability issues more clear. I may expand this list as I gain
;; faith in the mode and its language servers.
;;
;;; Code:
(when (require 'lsp-mode)
  (require 'lsp-ui)
  (require 'company-lsp)
  (require 'lsp-treemacs)
  ;;(autoload 'lsp-ui-mode "lsp-ui")
  ;;(autoload 'company-lsp "company-lsp")
  ;;(autoload 'lsp-treemacs-errors-list "lsp-treemacs")

  (when (require 'lsp-fsharp)
    (setq lsp-fsharp-server-path (concat (getenv "HOME") "/.FsAutoComplete/fsautocomplete.exe")
          lsp-fsharp-server-runtime 'mono)
    (add-hook 'fsharp-mode-hook 'lsp)))

;;; kqr-lsp.el ends here
