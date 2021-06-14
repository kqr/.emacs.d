(when (require 'notmuch nil 'noerror)
  (define-key global-map (kbd "<f5>") 'notmuch)

  (defun notmuch-toggle-deleted-tag (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted") beg end)
      (notmuch-search-tag (list "+deleted") beg end)))
  (define-key notmuch-search-mode-map "k" #'notmuch-toggle-deleted-tag)

  (defun notmuch-toggle-spam-tag (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "spam" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-spam") beg end)
      (notmuch-search-tag (list "+spam" "-inbox" "-unread") beg end)))
  (define-key notmuch-search-mode-map "x" #'notmuch-toggle-spam-tag)

  (setq notmuch-search-line-faces '(("unread" :weight bold))
        notmuch-show-indent-messages-width 4
        notmuch-search-oldest-first nil
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-poll-script nil)

  (cl-labels ((ebrela (gh hu us st tg)
                      (concat us (cons ?\100 nil)
                              hu (list ?\x2e)
                              tg)))
    (let ((a (ebrela "word" "xkqr" "a" "fz" "org"))
          (k (ebrela "spam" "rdw" "k" "protection" "se"))
          (s (ebrela "i" "kth" "stjernl" "hope" "se")))
      (setq notmuch-fcc-dirs
            (list (cons a (concat a "/Sent"))
                  (cons k (concat k "/Sent"))
                  (cons s (concat s "/Sent"))
                  (cons ".*" "sent")))))

  (setq notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-search
          notmuch-hello-insert-recent-searches
          notmuch-hello-insert-alltags
          notmuch-hello-insert-footer))

  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "unread" :query "tag:unread OR (tag:spam AND tag:unread)" :key "u")
          (:name "spam" :query "tag:spam" :key "m")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "all mail" :query "*" :key "a")))

  ;;;;;; Message mode for composing emails
  (setq message-cite-function 'message-cite-original-without-signature
        message-citation-line-function 'message-insert-formatted-citation-line
        message-cite-reply-position 'traditional
        message-yank-prefix "> "
        message-yank-cited-prefix ">"
        message-yank-empty-prefix ">"
        message-citation-line-format "%N (%n) %Y-%m-%d:"
        message-auto-save-directory "~/mail/drafts"
        message-default-mail-headers "Cc: \nBcc: \n"
        message-kill-buffer-on-exit t
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail
        message-confirm-send t
        mail-specify-envelope-from t
        send-mail-function 'smtpmail-send-it)
  (setq message-hidden-headers
        '("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From"))

  (defun message-split-quote ()
    (interactive)
    (save-excursion
      (message-beginning-of-line)
      (when (equal (following-char) ?>)
        (re-search-backward "^[^>]")
        (let ((from (point))
              (to (progn (end-of-visual-line) (point))))
          (copy-region-as-kill from to))))
    (insert "\n\n")
    (yank)
    (insert "\n> "))
  (define-key message-mode-map (kbd "C-M-s") 'message-split-quote)

  ;; Always sign outgoing messages
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
  ;; Use the sender name to find a sign key. (Required in message-mode in Emacs 27)
  (setq mml-secure-openpgp-sign-with-sender t)

  ;;;;;; Sendmail integration
  (when (require 'sendmail nil)
    (setq mail-envelope-from 'header
          sendmail-program "sendmail")))
