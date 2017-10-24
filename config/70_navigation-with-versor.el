(use-package versor
  :ensure nil
  :load-path "~/.emacs.d/config/libs/emacs-versor/lisp"
  :pin manual

  :config
  (customize-set-variable 'versor-auto-change-for-modes nil)
  (customize-set-variable 'versor-move-out-when-at-end nil)
  (customize-set-variable 'versor-level-wrap nil)
  (customize-set-variable 'versor-meta-level-wrap nil)
  (customize-set-variable 'versor-text-level 2)
  (customize-set-variable 'versor-text-meta-level 4)
  (customize-set-variable 'versor-non-text-level 2)
  (customize-set-variable 'versor-non-text-meta-level 6)

  (customize-set-variable
   'versor-meta-dimensions-valid-for-modes
   '((t t "cartesian" "structural" "text" "structured text" "program")))

  (customize-set-variable
   'versor-mode-current-levels
   (mapcar #'versor-mode-levels-triplet
           '((emacs-lisp-mode "structural" "exprs")
             (lisp-interaction-mode "structural" "exprs")
             (c-mode "program" "statement-parts")
             (text-mode "text" "words")
             (message-mode "text" "words")
             (org-mode "text" "words"))))

  (versor-setup
   'arrows
   'arrows-misc
   'meta
   'ctrl-x
   'text-in-code
   'quiet-underlying
   'local)

  (let ((color (face-attribute 'show-paren-match :background)))
    (seq-doseq (metamovemap (seq-subseq moves-moves 1))
      (seq-doseq (movemap (seq-subseq metamovemap 1))
        (versor-define-move movemap 'color color)
        (versor-define-move movemap :background color)))))
