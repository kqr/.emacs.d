;;; init-local --- Host-local configuration patches
;;; Commentary:
;;; Code:

(when (and (display-graphic-p)
           (find-font (font-spec :name "Hack")))
  (set-frame-font (font-spec :name "Hack" :size 14) t t))

(when (require 'notmuch nil 'noerror)
  (setq-default notmuch-command "notmuch"))

(when (require 'sendmail nil 'noerror)
  (setq-default sendmail-program "sendmail"))

(provide 'init-local)
;;; init-local.el ends here
