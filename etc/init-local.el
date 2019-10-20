;;; init-local --- Host-local configuration patches
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (set-frame-font (font-spec :name "Input Sans Light" :size 14) t t))

(when (require 'notmuch nil 'noerror)
  (setq-default notmuch-command "notmuch.sh"))

(when (require 'sendmail nil 'noerror)
  (setq-default sendmail-program "sendmail.sh"))

(provide 'init-local)
;;; init-local.el ends here
