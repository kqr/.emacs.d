(when (require 'highlight-parentheses)
  (setq hl-paren-colors '("#ff7328" "#f99759" "#f2a06d" "#eaa472"))
  (setq hl-paren-background-colors 'nil)

  ;; Disabling this temporarily to see if it helps with the input latency issues
  (global-highlight-parentheses-mode +1))

(run-with-idle-timer
 5 nil
 (lambda ()
   (when (require 'smartparens nil)
     (remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)
     (diminish 'smartparens-mode)
     (require 'smartparens-config)
     (smartparens-global-strict-mode)
     (sp-use-smartparens-bindings)
     (sp-local-pair 'ada-mode "'" nil :actions nil)
     (sp-local-pair 'fsharp-mode "'" nil :actions nil)
     (sp-local-pair 'js-jsx-mode "<" ">" :actions nil)
     (define-key smartparens-mode-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
     (define-key smartparens-mode-map (kbd "M-s") 'sp-split-sexp)
     (define-key smartparens-mode-map (kbd "M-r") 'sp-join-sexp))))
