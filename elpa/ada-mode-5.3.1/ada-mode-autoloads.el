;;; ada-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ada-mode" "ada-mode.el" (23296 19949 366466
;;;;;;  882000))
;;; Generated autoloads from ada-mode.el

(autoload 'ada-parse-prj-file "ada-mode" "\
Read Emacs Ada or compiler-specific project file PRJ-FILE, set project properties in `ada-prj-alist'.

\(fn PRJ-FILE)" nil nil)

(autoload 'ada-add-extensions "ada-mode" "\
Define SPEC and BODY as being valid extensions for Ada files.
SPEC and BODY are two regular expressions that must match against
the file name.

\(fn SPEC BODY)" nil nil)

(autoload 'ada-mode "ada-mode" "\
The major mode for editing Ada code.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ada-wisi-opentoken" "ada-wisi-opentoken.el"
;;;;;;  (23296 19949 994449 943000))
;;; Generated autoloads from ada-wisi-opentoken.el

(autoload 'ada-indent-opentoken-mode "ada-wisi-opentoken" "\
Minor mode for indenting grammar definitions for the OpenToken package.
Enable mode if ARG is positive

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "gpr-mode" "gpr-mode.el" (23296 19951 46421
;;;;;;  650000))
;;; Generated autoloads from gpr-mode.el

(autoload 'gpr-mode "gpr-mode" "\
The major mode for editing GNAT project files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-mode))

;;;***

;;;### (autoloads nil nil ("ada-build.el" "ada-fix-error.el" "ada-gnat-compile.el"
;;;;;;  "ada-gnat-xref.el" "ada-gps.el" "ada-grammar-wy.el" "ada-imenu.el"
;;;;;;  "ada-indent-user-options.el" "ada-mode-compat.el" "ada-mode-pkg.el"
;;;;;;  "ada-prj.el" "ada-skel.el" "ada-stmt.el" "ada-wisi.el" "ada-xref.el"
;;;;;;  "gnat-core.el" "gpr-grammar-wy.el" "gpr-query.el" "gpr-skel.el"
;;;;;;  "gpr-wisi.el") (23296 19950 294441 873000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ada-mode-autoloads.el ends here
