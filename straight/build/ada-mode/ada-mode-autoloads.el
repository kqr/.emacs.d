;;; ada-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ada-build" "ada-build.el" (0 0 0 0))
;;; Generated autoloads from ada-build.el

(autoload 'ada-build-check "ada-build" "\
Run the check_cmd project variable.
By default, this checks the current file for syntax errors.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-make "ada-build" "\
Run the make_cmd project variable.
By default, this compiles and links the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-set-make "ada-build" "\
Set the main project variable to the current file, then run the make_cmd project variable.
By default, this compiles and links the new main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(autoload 'ada-build-run "ada-build" "\
Run the run_cmd project variable.
By default, this runs the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command.

\(fn &optional CONFIRM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-build" '("ada-build-")))

;;;***

;;;### (autoloads nil "ada-fix-error" "ada-fix-error.el" (0 0 0 0))
;;; Generated autoloads from ada-fix-error.el

(autoload 'ada-fix-compiler-error "ada-fix-error" "\
Attempt to fix the current compiler error. Leave point at fixed code.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-fix-error" '("ada-")))

;;;***

;;;### (autoloads nil "ada-gnat-compile" "ada-gnat-compile.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ada-gnat-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-gnat-compile" '("ada-gnat-")))

;;;***

;;;### (autoloads nil "ada-gnat-xref" "ada-gnat-xref.el" (0 0 0 0))
;;; Generated autoloads from ada-gnat-xref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-gnat-xref" '("ada-gnat-")))

;;;***

;;;### (autoloads nil "ada-imenu" "ada-imenu.el" (0 0 0 0))
;;; Generated autoloads from ada-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-imenu" '("ada--imenu-")))

;;;***

;;;### (autoloads nil "ada-indent-user-options" "ada-indent-user-options.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ada-indent-user-options.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-indent-user-options" '("ada-")))

;;;***

;;;### (autoloads nil "ada-mode" "ada-mode.el" (0 0 0 0))
;;; Generated autoloads from ada-mode.el

(autoload 'ada-parse-prj-file "ada-mode" "\
Read Emacs Ada or compiler-specific project file PRJ-FILE, set project properties in `ada-prj-alist'.

\(fn PRJ-FILE)" nil nil)

(autoload 'ada-select-prj-file "ada-mode" "\
Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first.

\(fn PRJ-FILE &optional NO-FORCE)" t nil)

(autoload 'ada-add-extensions "ada-mode" "\
Define SPEC and BODY as being valid extensions for Ada files.
SPEC and BODY are two regular expressions that must match against
the file name.

\(fn SPEC BODY)" nil nil)

(autoload 'ada-mode "ada-mode" "\
The major mode for editing Ada code.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-mode" '("ada-" "wisi-inhibit-parse" "which-func-")))

;;;***

;;;### (autoloads nil "ada-process" "ada-process.el" (0 0 0 0))
;;; Generated autoloads from ada-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-process" '("ada-process-")))

;;;***

;;;### (autoloads nil "ada-project" "ada-project.el" (0 0 0 0))
;;; Generated autoloads from ada-project.el

(autoload 'ada-project-current "ada-project" "\
Return the project the user has set in `ada-project-current'.
For `project-find-functions'.

\(fn DIR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-project" '("ada-project-current" "refresh-project")))

;;;***

;;;### (autoloads nil "ada-skel" "ada-skel.el" (0 0 0 0))
;;; Generated autoloads from ada-skel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-skel" '("ada-skel-")))

;;;***

;;;### (autoloads nil "ada-wisi" "ada-wisi.el" (0 0 0 0))
;;; Generated autoloads from ada-wisi.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ada-wisi" '("ada-")))

;;;***

;;;### (autoloads nil "gnat-core" "gnat-core.el" (0 0 0 0))
;;; Generated autoloads from gnat-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnat-core" '("ada-gnat-" "gpr-query--sessions" "gnat")))

;;;***

;;;### (autoloads nil "gpr-indent-user-options" "gpr-indent-user-options.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gpr-indent-user-options.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-indent-user-options" '("gpr-indent")))

;;;***

;;;### (autoloads nil "gpr-mode" "gpr-mode.el" (0 0 0 0))
;;; Generated autoloads from gpr-mode.el

(autoload 'gpr-mode "gpr-mode" "\
The major mode for editing GNAT project files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-mode" '("gpr-")))

;;;***

;;;### (autoloads nil "gpr-process" "gpr-process.el" (0 0 0 0))
;;; Generated autoloads from gpr-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-process" '("gpr-process-")))

;;;***

;;;### (autoloads nil "gpr-query" "gpr-query.el" (0 0 0 0))
;;; Generated autoloads from gpr-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-query" '("ada-gpr-query" "gpr-")))

;;;***

;;;### (autoloads nil "gpr-skel" "gpr-skel.el" (0 0 0 0))
;;; Generated autoloads from gpr-skel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-skel" '("gpr-skel-" "skeleton-")))

;;;***

;;;### (autoloads nil "gpr-wisi" "gpr-wisi.el" (0 0 0 0))
;;; Generated autoloads from gpr-wisi.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gpr-wisi" '("gpr-")))

;;;***

;;;### (autoloads nil "xref-ada" "xref-ada.el" (0 0 0 0))
;;; Generated autoloads from xref-ada.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xref-ada" '("xref-ada-")))

;;;***

;;;### (autoloads nil nil ("ada-mode-compat.el" "ada-prj.el" "ada-stmt.el"
;;;;;;  "ada-xref.el") (0 0 0 0))

;;;***

(provide 'ada-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ada-mode-autoloads.el ends here
