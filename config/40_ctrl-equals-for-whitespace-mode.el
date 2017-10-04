(defun toggle-showparen-with-whitespace (arg)
  "Ensures show-paren-mode is off when whitespace-mode is turned on."
  (if (bound-and-true-p whitespace-mode)
      (show-paren-mode -1)
    (show-paren-mode +1)))

(advice-add 'whitespace-mode :after #'toggle-showparen-with-whitespace)

(bind-key* "C-=" 'whitespace-mode)

(defvar whitespace-style)
(setq whitespace-style
      '(face trailing tabs spaces newline space-mark tab-mark newline-mark))
(defvar whitespace-display-mappings)
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (tab-mark 9 [187 9] [92 9])
        (newline-mark 10 [182 10])))
