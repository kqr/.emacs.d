;;; hamlet-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hamlet-mode" "hamlet-mode.el" (0 0 0 0))
;;; Generated autoloads from hamlet-mode.el

(autoload 'hamlet-mode "hamlet-mode" "\
Major mode for editing Hamlet files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hamlet-mode" '("hamlet")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hamlet-mode-autoloads.el ends here
