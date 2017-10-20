(add-to-list 'load-path "config/libs/emacs-versor/lisp")

(use-package versor
  :config
  (versor-setup
   'arrows
   'arrows-misc
   'meta
   'ctrl-x
   'text-in-code
   'quiet-underlying)

  (customize-set-variable 'versor-auto-change-for-modes nil)
  ;;  (customize-set-variable 'versor-change-cursor-color nil)
  (customize-set-variable 'versor-move-out-when-at-end nil)
  (customize-set-variable 'versor-level-wrap nil)
  (customize-set-variable 'versor-meta-level-wrap nil)
  
  (seq-doseq (metamovemap (seq-subseq moves-moves 1))
    (seq-doseq (movemap (seq-subseq metamovemap 1))
      (versor-define-move movemap 'color (face-attribute 'show-paren-match :background))
      (versor-define-move movemap :background (face-attribute 'show-paren-match :background)))))
