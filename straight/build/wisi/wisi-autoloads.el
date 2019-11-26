;;; wisi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "wisi" "wisi.el" (0 0 0 0))
;;; Generated autoloads from wisi.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wisi" '("wisi-")))

;;;***

;;;### (autoloads nil "wisi-fringe" "wisi-fringe.el" (0 0 0 0))
;;; Generated autoloads from wisi-fringe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wisi-fringe" '("wisi-fringe-")))

;;;***

;;;### (autoloads nil "wisi-parse-common" "wisi-parse-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisi-parse-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wisi-parse-common" '("wisi-")))

;;;***

;;;### (autoloads nil "wisi-process-parse" "wisi-process-parse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisi-process-parse.el

(autoload 'wisi-process-parse-get "wisi-process-parse" "\
Return a ‘wisi-process--parser’ object matching PARSER label.
If label found in ‘wisi-process--alist’, return that.
Otherwise add PARSER to ‘wisi-process--alist’, return it.

\(fn PARSER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wisi-process-parse" '("wisi-")))

;;;***

;;;### (autoloads nil "wisitoken-parse_table-mode" "wisitoken-parse_table-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisitoken-parse_table-mode.el

(autoload 'wisitoken-parse_table-mode "wisitoken-parse_table-mode" "\
Provides navigation in wisi-generate parse table output.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.parse_table.*\\'" . wisitoken-parse_table-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wisitoken-parse_table-mode" '("wisitoken-parse_table--xref-backend")))

;;;***

(provide 'wisi-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wisi-autoloads.el ends here
