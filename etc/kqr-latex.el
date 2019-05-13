;; TODO verify what's required for this that I'm missing
;;(require 'tex-site nil)

(setq-default font-latex-deactivated-keyword-classes
              '("textual" "type-command" "type-declaration"))

(defun configure-latex ()
  "Configure AUCTeX in mode hooks."
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-auto-save t)
  (require 'latex-preview-pane)
  (latex-preview-pane-mode))

(add-hook 'LaTeX-mode-hook #'configure-latex)
