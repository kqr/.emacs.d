;;; meghanada-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "company-meghanada" "company-meghanada.el"
;;;;;;  (23256 31904 13861 740000))
;;; Generated autoloads from company-meghanada.el

(autoload 'meghanada-company-enable "company-meghanada" "\
Enable auto completion with company.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "eldoc-meghanada" "eldoc-meghanada.el" (23256
;;;;;;  31904 81861 690000))
;;; Generated autoloads from eldoc-meghanada.el

(autoload 'eldoc-meghanada-setup "eldoc-meghanada" "\
Set up eldoc function and enable 'eldoc-mode'.

\(fn)" t nil)

(autoload 'meghanada-eldoc-enable "eldoc-meghanada" "\
Enable eldoc for meghanada-mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "flycheck-meghanada" "flycheck-meghanada.el"
;;;;;;  (23256 31904 125861 657000))
;;; Generated autoloads from flycheck-meghanada.el

(autoload 'meghanada-flycheck-enable "flycheck-meghanada" "\
Enable flycheck for meghanada-mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "meghanada" "meghanada.el" (23256 31903 973861
;;;;;;  771000))
;;; Generated autoloads from meghanada.el

(autoload 'meghanada-install-server "meghanada" "\
Install meghanada-server's jar file from bintray .

\(fn)" t nil)

(autoload 'meghanada-update-server "meghanada" "\
Update meghanada-server's jar file from bintray .

\(fn)" t nil)

(autoload 'meghanada-server-start "meghanada" "\
TODO: FIX DOC .

\(fn)" t nil)

(autoload 'meghanada-server-kill "meghanada" "\
TODO: FIX DOC .

\(fn)" t nil)

(autoload 'meghanada-client-direct-connect "meghanada" "\
TODO: FIX DOC .

\(fn)" t nil)

(autoload 'meghanada-client-connect "meghanada" "\
TODO: FIX DOC .

\(fn)" t nil)

(autoload 'meghanada-client-disconnect "meghanada" "\
TODO: FIX DOC .

\(fn)" t nil)

(autoload 'meghanada-restart "meghanada" "\
Restart meghanada server and client.

\(fn)" t nil)

(autoload 'meghanada-mode "meghanada" "\
A better java development mode for Emacs (minor-mode).
\\{meghanada-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("meghanada-pkg.el") (23256 31904 45861
;;;;;;  717000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; meghanada-autoloads.el ends here
