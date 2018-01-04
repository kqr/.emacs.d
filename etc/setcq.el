;;; setcq --- Provide a convenient macro for customization
;;; Commentary:
;;
;; I used to have this in my init.el but then files like init-local.el triggered
;; complaints that the function was not guaranteed to be known, so I am extracting
;; it to a separate library that can be required in each file that uses it.
;;
;;; Code:

(defmacro setcq (symbol &rest args)
  "A convenient wrapper around (customize-set-variable 'SYMBOL ARGS)."
  (append `(customize-set-variable (quote ,symbol)) args))

(provide 'setcq)
;;; setcq.el ends here
