(when (require 'paren nil)
  (show-paren-mode +1)
  (setq-default show-paren-delay 0
                show-paren-when-point-inside-paren t
                show-paren-style 'expression)
  ;; This is in order for region to take priority over show-paren highlighting
  (setq-default show-paren-priority -200))

(when (require 'highlight-parentheses nil)
  (setq hl-paren-colors '("#ff7328" "#f99759" "#f2a06d" "#eaa472"))
  (setq hl-paren-background-colors 'nil)
  (global-highlight-parentheses-mode +1))

(run-with-idle-timer
 5 nil
 (lambda ()
   (when (require 'smartparens nil)
     (diminish 'smartparens-mode)
     (require 'smartparens-config)
     (smartparens-global-strict-mode)
     (sp-use-smartparens-bindings)
     (sp-local-pair 'ada-mode "'" nil :actions nil)
     (sp-local-pair 'fsharp-mode "'" nil :actions nil)
     (sp-local-pair 'fsharp-mode "<" ">")
     (sp-local-pair 'csharp-mode "<" ">")
     (define-key smartparens-mode-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
     (define-key smartparens-mode-map (kbd "M-s") 'sp-split-sexp)
     (define-key smartparens-mode-map (kbd "M-r") 'sp-join-sexp))))
