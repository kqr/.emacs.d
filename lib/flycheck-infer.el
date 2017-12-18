;;; flycheck-infer.el --- Support infer in flycheck

;;; Commentary:

;; Use infer as a java static analyser


;;; Code:
(require 'flycheck)

(flycheck-define-checker javac-infer
    "A Java syntax and style checker using infer.

See URL http://fbinfer.com/"
    :command ("jinfer.sh")
    :error-patterns
    ((error line-start (file-name) ":" line ":" " error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" " warning:" (message) line-end)
     (info line-start (file-name) ":" line ":" " info:" (message) line-end))
    :modes java-mode)


(add-to-list 'flycheck-checkers 'javac-infer)

(provide 'flycheck-infer)
;;; flycheck-infer.el ends here
