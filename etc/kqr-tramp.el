(with-eval-after-load "tramp"
  (defun tramp-file-name-as-hop (vec)
    "Return VEC formatted as a hop."
    (concat (tramp-file-name-hop vec)
            (tramp-file-name-method vec)
            (and (tramp-file-name-method vec)
                 tramp-postfix-method-format)
            (tramp-file-name-user-domain vec)
            (and (tramp-file-name-user-domain vec)
                 tramp-postfix-user-format)
            (tramp-file-name-host-port vec)
            tramp-postfix-hop-format))

  (defun sudo-edit-current-file (as-user)
    (interactive "sAs user? ")
    (let ((position (point)))
      (find-alternate-file
       (if (file-remote-p (buffer-file-name))
           (with-parsed-tramp-file-name buffer-file-name remote
             (tramp-make-tramp-file-name
              "sudo" as-user remote-domain
              remote-host remote-port
              remote-localname
              (tramp-file-name-as-hop
               (tramp-dissect-file-name
                (buffer-file-name)))))
         (concat "/sudo:root@localhost:"
                 (buffer-file-name))))
      (goto-char position))))
