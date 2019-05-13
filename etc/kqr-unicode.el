;; Replace keywords with Unicode symbols
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)
(defun prettify-programming-symbols ()
  "Prettify programming symbols!"
  (interactive)
  (let ((local-symbols
         (pcase major-mode
           ('fsharp-mode '((">>" . ?»)))
           ('csharp-mode '(("=>" . ?⤇)
                           ("foreach" . ?∀)))
           (_ ())))
        (global-symbols
         '(;; Common operators
           ("==" . (?＝ (Bc . Bl) ?＝))  ;; ==
           ("!=" . (?＝ (Bc . Bc) ?/))  ;; =/=
           (">=" . ?≥)
           ("<=" . ?≤)
           ("->" . ?⟼)
           ("<-" . ?⟵)
           ("|>" . ?▷)
           ("<|" . ?◁)
           ("||" . ?∨)
           ("&&" . ?∧)
           ("!" . ?¬)

           ;; Common types
           ;; Some of these are known under multiple names, but care should be
           ;; taken to only prettify ONE of them per mode, or things get
           ;; confusing. The choice here is strongly influenced by my current
           ;; day job in .NET, mainly C#.
           ("void" . ?∅)
           ("bool" . ?𝔹)  ;; aka boolean, Bool, Boolean – but pick just one
           ("unsigned" . ?ℕ)
           ("int" . ?ℤ)
           ("float" . ?ℝ)
           ("double" . (?ℝ (Br . Bc) ?ℝ))  ;; RR
           ("char" . ?Σ)
           ("string" . (?Σ (tr . cl) ?*))  ;; Σ*

           ;; Greek
           ("alpha" . ?α)
           ("beta" . ?β)
           ("gamma" . ?γ)
           ("Gamma" . ?Γ)
           ("delta" . ?δ)
           ("Delta" . ?Δ)
           ("lambda" . ?λ)
           ("sigma" . ?σ)
           ("Sigma" . ?Σ)
           ("pi" . ?π)
           ("tau" . ?τ)
           ("psi" . ?ψ)
           ("Psi" . ?Ψ)
           ("Phi" . ?Φ))))
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          (append local-symbols global-symbols))))
(add-hook 'prog-mode-hook 'prettify-programming-symbols)
(add-hook 'ess-mode-hook 'prettify-programming-symbols)

;; Allow convenient hotkeys for special characters I use often
(with-eval-after-load 'iso-transl
  (mapc (lambda (mapping)
          (message "" mapping)
          (define-key iso-transl-ctl-x-8-map
            (car mapping) (cdr mapping)))
        '((" " . "\ufeff")
          ("." . "…")
          ("m" . "·")
          ("s" . "§")
          ("p" . "¶"))))
