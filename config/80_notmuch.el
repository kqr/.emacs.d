;; Load the locally installed notmuch mode to ensure versions match.
(when (require 'notmuch nil :noerror)

  (setq message-auto-save-directory "~/mail/drafts")
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq message-kill-buffer-on-exit t)
  (setq message-sendmail-envelope-from 'header)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-confirm-send t)
  (setq message-hidden-headers
	'("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From"))

  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)

  (setq send-mail-function 'smtpmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")

  (setq notmuch-archive-tags '("-inbox" "-unread"))

  (defun notmuch-toggle-deleted-tag (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted") beg end)
      (notmuch-search-tag (list "+deleted") beg end)))
  (define-key notmuch-search-mode-map "k" #'notmuch-toggle-deleted-tag)

  (setq notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-search
          notmuch-hello-insert-recent-searches
          notmuch-hello-insert-alltags
          notmuch-hello-insert-footer))
  (setq notmuch-poll-script nil)

  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "all mail" :query "*" :key "a")))

  (setq notmuch-search-line-faces '(("unread" :weight bold)))

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-indent-messages-width 4))


