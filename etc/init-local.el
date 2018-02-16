;;; init-local --- Host-local configuration patches
;;; Commentary:
;;; Code:

(when (require 'notmuch nil 'noerror)
  (setq-default notmuch-command "notmuch.sh"))

(when (require 'sendmail nil 'noerror)
  (setq-default sendmail-program "sendmail.sh"))

(provide 'init-local)
;;; init-local.el ends here
