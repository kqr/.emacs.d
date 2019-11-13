(let ((fallback-font "Noto Sans"))
  (if (not (member fallback-font (font-family-list)))
      (warn (concat fallback-font " not among installed fonts. Symbols may behave weirdly."))
    ;; Noto likely gets much more love in the non-Latin 1 ranges of Unicode.
    (set-fontset-font t nil fallback-font nil 'prepend)
    (set-fontset-font t 'symbol fallback-font)
    (set-fontset-font t 'mathematical fallback-font)))

;; Replace keywords with Unicode symbols
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun refresh-programming-symbols ()
  "Run this command when prettify-programming-symbols has been re-evaled."
  (interactive)
  (prettify-programming-symbols)
  (global-prettify-symbols-mode -1)
  (global-prettify-symbols-mode +1))

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
           (">>=" . (?\s (Bl . Bl) ?> (Br . Bc) ?> (Br . Bc) ?=))
           ("<*>" . (?< (Bc . Bl) ?· (Br . Bc) ?>))
           ("*>" . (?· (Br . Bc) ?\s (Bc . Bl) ?>))
           ("<*" . (?< (Br . Bc) ?\s (Bc . Bl) ?·))
           ("<|>" . (?< (Bc . Bl) ?| (Br . Bc) ?>))
           (":>" . (?: (Br . Bc) ?\s (Bc . Bl) ?>))
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
  (defvar iso-transl-greek-map (make-sparse-keymap))
  (mapc (lambda (mapping)
          (define-key iso-transl-greek-map (kbd (car mapping)) (cdr mapping)))
        '(("a" . "α")
          ("b" . "β")
          ("c" . "χ")
          ("D" . "Δ")
          ("d" . "δ")
          ("e" . "ε")
          ("F" . "Φ")
          ("f" . "φ")
          ("G" . "Γ")
          ("g" . "γ")
          ("H" . "Θ")
          ("h" . "θ")
          ("k" . "κ")
          ("L" . "Λ")
          ("l" . "λ")
          ("m" . "μ")
          ("p" . "π")
          ("r" . "ρ")
          ("S" . "Σ")
          ("s" . "σ")
          ("t" . "τ")
          ("W" . "Ω")
          ("w" . "ω")
          ("x" . "ξ")))
  (define-key global-map (kbd "C-x g") iso-transl-greek-map)

  (define-key global-map (kbd "C-x m") nil)
  (defvar iso-transl-math-map (make-sparse-keymap))
  (mapc (lambda (mapping)
          (define-key iso-transl-math-map (kbd (car mapping)) (cdr mapping)))
        '(("2" . "²")
          ("3" . "³")
          ("g" . "≥")
          ("l" . "≤")
          ("p" . "±")
          ("q" . "√")
          ("d" . "∂")
          ("x" . "×")
          ("i" . "∫")))
  (define-key global-map (kbd "C-x m") iso-transl-math-map)

  (mapc (lambda (mapping)
          (define-key iso-transl-ctl-x-8-map (kbd (car mapping)) (cdr mapping)))
        '((" " . "\ufeff")
          ("." . "…")
          ("m" . "·")
          ("s" . "§")
          ("p" . "¶"))))
