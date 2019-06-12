(autoload 'csharp-mode "csharp-mode")
(autoload 'fsharp-mode "fsharp-mode")
(push '("\\.cs\\'" . csharp-mode) auto-mode-alist)
(push '("\\.fs\\'" . fsharp-mode) auto-mode-alist)
(push '("\\..sproj\\'" . nxml-mode) auto-mode-alist)

(defun configure-omnisharp ()
  "Set up omnisharp for C# and F# the way I'm used to."
  (when (require 'omnisharp nil)
    (when (require 'company nil)
      (add-to-list 'company-backends #'company-omnisharp))

    (setq omnisharp-expected-server-version "1.32.8")

    (defun omnisharp-enable ()
      "Configure advanced settings related to C# development."
      (omnisharp-mode)
      (local-set-key (kbd "C-c r m") 'omnisharp-run-code-action-refactoring)
      (local-set-key (kbd "C-c r r") 'omnisharp-rename)
      (local-set-key (kbd "C-c r d") 'omnisharp-go-to-definition-other-window)
      (local-set-key (kbd "C-c r t") 'omnisharp-current-type-information)
      (local-set-key (kbd "C-c r u") 'omnisharp-find-usages)
      (local-set-key (kbd "C-c r i") 'omnisharp-find-implementations))
    t))

(with-eval-after-load "csharp-mode"
  (defun csharp-mode-enable ()
    "Configure settings relating to C# development."
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (c-set-offset 'arglist-close 0)
    (c-set-offset 'brace-list-open '-)

    (setq tab-width 4)
    (electric-indent-local-mode -1)
    (c-set-offset 'inline-open 0)
    (when (configure-omnisharp)
      (add-hook 'csharp-mode-hook 'omnisharp-enable)))
  (add-hook 'csharp-mode-hook 'csharp-mode-enable))

(with-eval-after-load "fsharp-mode"
  (defun fsharp-mode-enable ()
    (push 'fsharp-mode aggressive-indent-excluded-modes)

    (when (configure-omnisharp)
      (add-hook 'fsharp-mode-hook 'omnisharp-enable)))
  (add-hook 'fsharp-mode-hook 'fsharp-mode-enable))
