;; ada-gnat-compile.el --- Ada mode compiling functionality provided by 'gnat'  -*- lexical-binding:t -*-
;; Includes related functions, such as gnatprep support.
;;
;; These tools are all Ada-specific; use Makefiles for multi-language
;; GNAT compilation tools.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2012 - 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Usage:
;;
;; Emacs should enter Ada mode automatically when you load an Ada
;; file, based on the file extension.
;;
;; By default, ada-mode is configured to load this file, so nothing
;; special needs to done to use it.

(require 'cl-lib)
(require 'compile)
(require 'gnat-core)
(require 'ada-fix-error)

;;;;; code

;;;; compiler message handling

(defun ada-gnat-compilation-filter ()
  "Filter to add text properties to secondary file references.
For `compilation-filter-hook'."
  (save-excursion
    (goto-char compilation-filter-start)

    ;; primary references are handled by font-lock functions; see
    ;; `compilation-mode-font-lock-keywords'.
    ;;
    ;; compilation-filter might insert partial lines, or it might insert multiple lines
    (goto-char (line-beginning-position))
    (while (not (eobp))
      ;; We don't want 'next-error' to always go to secondary
      ;; references, so we _don't_ set 'compilation-message text
      ;; property. Instead, we set 'ada-secondary-error, so
      ;; `ada-goto-secondary-error' will handle it. We also set
      ;; fonts, so the user can see the reference.

      ;; typical secondary references look like:
      ;;
      ;; trivial_productions_test.adb:57:77:   ==> in call to "Get" at \
      ;;    opentoken-token-enumerated-analyzer.ads:88, instance at line 41
      ;;
      ;; c:/foo/bar/lookahead_test.adb:379:14: found type access to "Standard.String" defined at line 379
      ;;
      ;; lookahead_test.ads:23:09: "Name" has been inherited from subprogram at aunit-simple_test_cases.ads:47
      ;;
      ;; lalr.adb:668:37: non-visible declaration at analyzer.ads:60, instance at parser.ads:38
      ;;
      ;; save the file from the primary reference, look for "*.ad?:nn", "at line nnn"

      (let (file)
	(when (looking-at "^\\(\\(.:\\)?[^ :\n]+\\):")
	  (setq file (match-string-no-properties 1)))

	(skip-syntax-forward "^-"); space following primary reference

	(while (search-forward-regexp "\\s-\\(\\([^[:blank:]]+\\.[[:alpha:]]+\\):\\([0-9]+\\)\\)"
				      (line-end-position) t)

	  (goto-char (match-end 0))
	  (with-silent-modifications
	    (compilation--put-prop 2 'font-lock-face compilation-info-face); file
	    (compilation--put-prop 3 'font-lock-face compilation-line-face); line
	    (put-text-property
	     (match-beginning 0) (match-end 0)
	     'ada-secondary-error
	     (list
	      (match-string-no-properties 2); file
	      (string-to-number (match-string-no-properties 3)); line
	      0)); Emacs column; zero indexed
	    ))

	(when (search-forward-regexp "\\(at line \\)\\([0-9]+\\)" (line-end-position) t)
	  (with-silent-modifications
	    (compilation--put-prop 1 'font-lock-face compilation-info-face); "at line" instead of file
	    (compilation--put-prop 2 'font-lock-face compilation-line-face); line
	    (put-text-property
	     (match-beginning 1) (match-end 1)
	     'ada-secondary-error
	     (list
	      file
	      (string-to-number (match-string-no-properties 2)); line
	      1)); column
	    ))
	(forward-line 1))
      )
    ))

(defun ada-gnat-debug-filter ()
  ;; call ada-gnat-compilation-filter with `compilation-filter-start' bound
  (interactive)
  (beginning-of-line)
  (let ((compilation-filter-start (point)))
    (ada-gnat-compilation-filter)))

;;;;; auto fix compilation errors

(defconst ada-gnat-quoted-name-regexp
  "\"\\([a-zA-Z0-9_.']+\\)\""
  "regexp to extract the quoted names in error messages")

(defconst ada-gnat-quoted-punctuation-regexp
  "\"\\([,:;=()|]+\\)\""
  "regexp to extract quoted punctuation in error messages")

(defvar ada-gnat-fix-error-hook nil
  "For `ada-fix-error-alist'.")

(defun ada-gnat-misspelling ()
  "Return correct spelling from current compiler error, if there are corrections offered.
Prompt user if more than one."
  ;; wisi-output.adb:115:41: no selector "Productions" for type "RHS_Type" defined at wisi.ads:77
  ;; wisi-output.adb:115:41: invalid expression in loop iterator
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;;
  ;; column number can vary, so only check the line number

  (let ((line (progn (beginning-of-line) (nth 1 (compilation--message->loc (ada-get-compilation-message)))))
	done choices)
    (while (not done)
      (forward-line 1)
      (setq done (or (not (ada-get-compilation-message))
		     (not (equal line (nth 1 (compilation--message->loc (ada-get-compilation-message)))))))
      (when (and (not done)
		 (progn
		   (skip-syntax-forward "^-")
		   (forward-char 1)
		   (looking-at (concat "possible misspelling of " ada-gnat-quoted-name-regexp))))
	(push (match-string 1) choices)))

    ;; return correct spelling
    (cond
     ((= 0 (length choices))
      nil)

     ((= 1 (length choices))
      (car choices))

     (t ;; multiple choices
      (completing-read "correct spelling: " choices))
     )))

(defun ada-gnat-fix-error (_msg source-buffer _source-window)
  "For `ada-gnat-fix-error-hook'."
  (let ((start-pos (point))
	message-column
	result)
    ;; Move to start of error message text
    (skip-syntax-forward "^-")
    (forward-char 1)
    (setq message-column (current-column))

    ;; recognize it, handle it
    (setq
     result
     (unwind-protect
	 (cond
	  ;; It is tempting to define an alist of (MATCH . ACTION), but
	  ;; that is too hard to debug
	  ;;
	  ;; This list will get long, so let's impose some order.
	  ;;
	  ;; First expressions that start with a named regexp, alphabetical by variable name.
	  ;;
	  ;; Then expressions that start with a string, alphabetical by string.
	  ;;
	  ;; Then style errors.

	  ((looking-at (concat ada-gnat-quoted-name-regexp " is not visible"))
	   (let ((done nil)
		 (file-line-struct (progn (beginning-of-line) (ada-get-compilation-message)))
		 pos choices unit-name)
	     ;; next line may contain a reference to where ident is
	     ;; defined; if present, it will have been marked by
	     ;; ada-gnat-compilation-filter:
	     ;;
	     ;; gnatquery.adb:255:13: "Has_Element" is not visible
	     ;; gnatquery.adb:255:13: non-visible declaration at a-convec.ads:68, instance at gnatcoll-arg_lists.ads:157
	     ;; gnatquery.adb:255:13: non-visible declaration at a-coorse.ads:62, instance at gnatcoll-xref.ads:912
	     ;; gnatquery.adb:255:13: non-visible declaration at a-coorse.ads:62, instance at gnatcoll-xref.ads:799
	     ;; gnatquery.adb:255:13: non-visible declaration at gnatcoll-xref.ads:314
	     ;;
	     ;; or the next line may contain "multiple use clauses cause hiding"
	     ;;
	     ;; the lines after that may contain alternate matches;
	     ;; collect all, let user choose.
	     (forward-line 1)
	     (unless (looking-at ".* multiple use clauses cause hiding")
	       (while (not done)
		 (let ((limit (1- (line-end-position))))
		   ;; 1- because next compilation error is at next line beginning
		   (setq done (not
			       (and
				(equal file-line-struct (ada-get-compilation-message))
				(setq pos (next-single-property-change (point) 'ada-secondary-error nil limit))
				(< pos limit))))
		   (when (not done)
		     (let* ((item (get-text-property pos 'ada-secondary-error))
			    (unit-file (nth 0 item))
                            (choice (ada-ada-name-from-file-name unit-file)))
                       (unless (member choice choices) (push choice choices))
		       (goto-char (1+ pos))
		       (goto-char (1+ (next-single-property-change (point) 'ada-secondary-error nil limit)))
		       (when (eolp) (forward-line 1))
		       ))
		   )));; unless while let

	     (setq unit-name
		   (cond
		    ((= 0 (length choices)) nil)
		    ((= 1 (length choices)) (car choices))
		    (t ;; multiple choices
		     (completing-read "package name: " choices))))

	     (when unit-name
	       (pop-to-buffer source-buffer)
	       ;; We either need to add a with_clause for a package, or
	       ;; prepend the package name here (or add a use clause, but I
	       ;; don't want to do that automatically).
	       ;;
	       ;; If we need to add a with_clause, unit-name may be only
	       ;; the prefix of the real package name, but in that case
	       ;; we'll be back after the next compile; no way to get the
	       ;; full package name (without the function/type name) now.
	       ;; Note that we can't use gnat find, because the code
	       ;; doesn't compile.
	       (cond
		((looking-at (concat unit-name "\\."))
		 (ada-fix-add-with-clause unit-name))
		(t
		 (ada-fix-insert-unit-name unit-name)
		 (insert ".")))
	       t) ;; success, else nil => fail
	     ))

	  ((or (looking-at (concat ada-gnat-quoted-name-regexp " is undefined"))
	       (looking-at (concat ada-gnat-quoted-name-regexp " is not a predefined library unit")))
	   ;; We either need to add a with_clause for a package, or
	   ;; something is spelled wrong.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (pop-to-buffer source-buffer)
		     (search-forward unit-name)
		     (replace-match correct-spelling))

		 ;; else assume missing with
		 (pop-to-buffer source-buffer)
		 (ada-fix-add-with-clause unit-name))))
	   t)

	  ((looking-at (concat ada-gnat-quoted-name-regexp " not declared in " ada-gnat-quoted-name-regexp))
	   (save-excursion
	     (let ((child-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (setq correct-spelling (match-string 1))
		     (pop-to-buffer source-buffer)
		     (search-forward child-name)
		     (replace-match correct-spelling))

		 ;; else guess that "child" is a child package, and extend the with_clause
		 (pop-to-buffer source-buffer)
		 (ada-fix-extend-with-clause child-name))))
	   t)

	  ((looking-at (concat ada-gnat-quoted-punctuation-regexp
			       " should be "
			       ada-gnat-quoted-punctuation-regexp))
	   (let ((bad (match-string-no-properties 1))
		 (good (match-string-no-properties 2)))
	     (pop-to-buffer source-buffer)
	     (looking-at bad)
	     (delete-region (match-beginning 0) (match-end 0))
	     (insert good))
	   t)

;;;; strings
	  ((looking-at (concat "\"end " ada-name-regexp ";\" expected"))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (if (looking-at (concat "end " ada-name-regexp ";"))
		 (progn
		   (goto-char (match-end 1))   ; just before ';'
		   (delete-region (match-beginning 1) (match-end 1)))
	       ;; else we have just 'end;'
	       (forward-word 1)
	       (insert " "))
	     (insert expected-name))
	   t)

	  ((looking-at (concat "\"end loop " ada-name-regexp ";\" expected"))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (if (looking-at (concat "end loop " ada-name-regexp ";"))
		 (progn
		   (goto-char (match-end 1))   ; just before ';'
		   (delete-region (match-beginning 1) (match-end 1)))
	       ;; else we have just 'end loop;'
	       (forward-word 2)
	       (insert " "))
	     (insert expected-name))
	   t)

	  ((looking-at "expected an access type")
	   (progn
	     (set-buffer source-buffer)
	     (backward-char 1)
	     (when (looking-at "\\.all")
	       (delete-char 4)
	       t)))

	  ((looking-at (concat "expected \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (forward-line 1)
	   (move-to-column message-column)
	   (cond
	    ((looking-at "found procedure name")
	     (pop-to-buffer source-buffer)
	     (forward-word 1)
	     (insert "'Access")
	     t)
	    ((looking-at "found type access")
	     (pop-to-buffer source-buffer)
	     (if (looking-at "'Access")
		 (kill-word 1)
	       (forward-word 1)
	       (insert ".all"))
	     t)
	    ((looking-at "found type .*_Access_Type")
	     ;; assume just need '.all'
	     (pop-to-buffer source-buffer)
	     (forward-word 1)
	     (insert ".all")
	     t)
	    ))

	  ((looking-at "extra \".\" ignored")
	   (set-buffer source-buffer)
	   (delete-char 1)
	   t)

	  ((looking-at (concat "keyword " ada-gnat-quoted-name-regexp " expected here"))
	   (let ((expected-keyword (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (insert " " expected-keyword))
	   t)

	  ((looking-at "\\(?:possible \\)?missing \"with \\([a-zA-Z0-9_.]+\\);")
	   ;; also 'possible missing "with Ada.Text_IO; use Ada.Text_IO"' - ignoring the 'use'
	   (let ((package-name (match-string-no-properties 1)))
	     (pop-to-buffer source-buffer)
	     ;; Could check if prefix is already with'd, extend
	     ;; it. But no one has reported that case yet; this
	     ;; message only occurs for predefined Ada packages.
	     (ada-fix-add-with-clause package-name))
	   t)

	  ;; must be after above
	  ((looking-at "missing \"\\(.+\\)\"")
	   (let ((stuff (match-string-no-properties 1)))
	     (set-buffer source-buffer)
	     (insert (concat stuff)));; if missing ")", don't need space; otherwise do?
	   t)

	  ((looking-at (concat "\\(?:possible \\)?misspelling of " ada-gnat-quoted-name-regexp))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (looking-at ada-name-regexp)
	     (delete-region (match-beginning 1) (match-end 1))
	     (insert expected-name))
	   t)

	  ((looking-at "No legal interpretation for operator")
	   (forward-line 1)
	   (move-to-column message-column)
	   (looking-at (concat "use clause on " ada-gnat-quoted-name-regexp))
	   (let ((package (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-fix-add-use package))
	   t)

	  ((looking-at (concat "no selector " ada-gnat-quoted-name-regexp))
	   ;; Check next line for spelling error.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (when correct-spelling
		 (pop-to-buffer source-buffer)
		 (search-forward unit-name)
		 (replace-match correct-spelling)
		 t))))

	  ((looking-at (concat "operator for \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (let ((type (match-string 2)))
	     (pop-to-buffer source-buffer)
	     (ada-fix-add-use-type type)
	   t))

	  ((looking-at "package \"Ada\" is hidden")
	   (pop-to-buffer source-buffer)
	   (forward-word -1)
	   (insert "Standard.")
	   t)

	  ((looking-at "parentheses required for unary minus")
	   (set-buffer source-buffer)
	   (insert "(")
	   (forward-word 1)
	   (insert ")")
	   t)

	  ((looking-at "prefix of dereference must be an access type")
	   (pop-to-buffer source-buffer)
	   ;; point is after '.' in '.all'
	   (delete-region (- (point) 1) (+ (point) 3))
	   t)

;;;; warnings
	  ((looking-at (concat "warning: " ada-gnat-quoted-name-regexp " is already use-visible"))
	   ;; just delete the 'use'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat "warning: " ada-gnat-quoted-name-regexp " is not modified, could be declared constant"))
	   (pop-to-buffer source-buffer)
	   (search-forward ":")
	   (forward-comment (- (point-max) (point)))
	   ;; "aliased" must be before "constant", so check for it
	   (when (looking-at "aliased")
	     (forward-word 1)
	     (forward-char 1))
	   (insert "constant ")
	   t)

	  ((looking-at (concat "warning: constant " ada-gnat-quoted-name-regexp " is not referenced"))
	   (let ((constant (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (end-of-line)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" constant ");"))
	   t)

	  ((looking-at (concat "warning: formal parameter " ada-gnat-quoted-name-regexp " is not referenced"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-goto-declarative-region-start)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((looking-at (concat "warning: formal parameter " ada-gnat-quoted-name-regexp " is not modified"))
	   (let ((mode-regexp "\"\\([in out]+\\)\"")
		 new-mode
		 old-mode)
	     (forward-line 1)
	     (search-forward-regexp
	      (concat "mode could be " mode-regexp " instead of " mode-regexp))
	     (setq new-mode (match-string 1))
	     (setq old-mode (match-string 2))
	     (pop-to-buffer source-buffer)
	     (search-forward old-mode)
	     (replace-match new-mode)
	     (ada-align)
	     )
	   t)

	  ((or
	    (looking-at (concat "warning: no entities of " ada-gnat-quoted-name-regexp " are referenced$"))
	    (looking-at (concat "warning: unit " ada-gnat-quoted-name-regexp " is never instantiated$"))
	    (looking-at "warning: redundant with clause"))
	   ;; just delete the 'with'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat "warning: variable " ada-gnat-quoted-name-regexp " is assigned but never read"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-goto-end) ;; leaves point before semicolon
	     (forward-char 1)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((looking-at (concat "warning: unit " ada-gnat-quoted-name-regexp " is not referenced$"))
	   ;; just delete the 'with'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

;;;; style errors
	  ((looking-at "(style) \".*\" in wrong column")
	   (set-buffer source-buffer)
	   (funcall indent-line-function)
	   t)

	  ((looking-at "(style) bad capitalization, mixed case required")
	   (set-buffer source-buffer)
	   (forward-word)
	   (ada-case-adjust-identifier)
	   t)

	  ((looking-at (concat "(style) bad casing of " ada-gnat-quoted-name-regexp))
	   (let ((correct (match-string-no-properties 1))
		 end)
	     ;; gnat leaves point on first bad character, but we need to replace the whole word
	     (set-buffer source-buffer)
	     (skip-syntax-backward "w_")
	     (setq end (point))
	     (skip-syntax-forward "w_")
	     (delete-region (point) end)
	     (insert correct))
	   t)

	  ((or
	    (looking-at "(style) bad column")
	    (looking-at "(style) bad indentation")
	    (looking-at "(style) incorrect layout"))
	   (set-buffer source-buffer)
	   (funcall indent-line-function)
	   t)

	  ((looking-at "(style) \"exit \\(.*\\)\" required")
	   (let ((name (match-string-no-properties 1)))
	     (set-buffer source-buffer)
	     (forward-word 1)
	     (insert (concat " " name))
	   t))

	  ((looking-at "(style) misplaced \"then\"")
	   (set-buffer source-buffer)
	   (delete-indentation)
	   t)

         ((looking-at "(style) missing \"overriding\" indicator")
          (set-buffer source-buffer)
          (cond
           ((looking-at "\\(procedure\\)\\|\\(function\\)")
            (insert "overriding ")
	    t)
           (t
            nil)))

	  ((looking-at "(style) reserved words must be all lower case")
	   (set-buffer source-buffer)
	   (downcase-word 1)
	   t)

	  ((looking-at "(style) space not allowed")
	   (set-buffer source-buffer)
	   ;; Error places point on space. More than one trailing space
	   ;; should be fixed by delete-trailing-whitespace in
	   ;; before-save-hook, once the file is modified.
	   (delete-char 1)
	   t)

	  ((looking-at "(style) space required")
	   (set-buffer source-buffer)
	   (insert " ")
	   t)
	  )));; end of setq unwind-protect cond
    (if result
	t
      (goto-char start-pos)
      nil)
    ))

;;;;; setup

(defun ada-gnat-compile-select-prj ()
  (setq ada-fix-error-hook 'ada-gnat-fix-error-hook)
  (setq ada-prj-show-prj-path 'gnat-prj-show-prj-path)
  (add-to-list 'completion-ignored-extensions ".ali") ;; gnat library files
  (add-hook 'ada-syntax-propertize-hook 'ada-gnat-syntax-propertize)
  (add-hook 'ada-syntax-propertize-hook 'gnatprep-syntax-propertize)
  (syntax-ppss-flush-cache (point-min));; force re-evaluate with hook.

  ;; There is no common convention for a file extension for gnatprep files.
  ;;
  ;; find error locations in .gpr files
  (setq compilation-search-path (append compilation-search-path (ada-prj-get 'prj_dir)))

  ;; ‘compilation-environment’ is buffer-local, but the user might
  ;; delete that buffer. So set both global and local.
  (let* ((process-environment (ada-prj-get 'proc_env))
	 (gpr-path (getenv "GPR_PROJECT_PATH"))
	 (comp-env (list (concat "GPR_PROJECT_PATH=" gpr-path)))
	 (comp-buf (get-buffer "*compilation*")))
    (when (buffer-live-p comp-buf)
      (with-current-buffer comp-buf
	(setenv "GPR_PROJECT_PATH" gpr-path)
	(setq compilation-environment comp-env)))
    (set-default 'compilation-environment comp-env)
    )

  ;; must be after indentation engine setup, because that resets the
  ;; indent function list.
  (add-hook 'ada-mode-hook 'gnatprep-setup t)

  (add-hook 'compilation-filter-hook 'ada-gnat-compilation-filter)

  ;; ada-mode.el project file parser sets this to other compilers used
  ;; in the project, so we only add here.
  (add-to-list 'compilation-error-regexp-alist 'gnat)
  )

(defun ada-gnat-compile-deselect-prj ()
  (setq ada-fix-error-hook nil)
  (setq completion-ignored-extensions (delete ".ali" completion-ignored-extensions))
  (setq ada-syntax-propertize-hook (delq 'gnatprep-syntax-propertize ada-syntax-propertize-hook))
  (setq ada-syntax-propertize-hook (delq 'ada-gnat-syntax-propertize ada-syntax-propertize-hook))
  (syntax-ppss-flush-cache (point-min));; force re-evaluate with hook.

  ;; don't need to delete from compilation-search-path; completely rewritten in ada-select-prj-file
  (setq compilation-environment nil)

  (setq ada-mode-hook (delq 'gnatprep-setup ada-mode-hook))

  (setq compilation-filter-hook (delete 'ada-gnat-compilation-filter compilation-filter-hook))
  (setq compilation-error-regexp-alist (delete 'gnat compilation-error-regexp-alist))
  )

(defun ada-gnat-compile ()
  "Set Ada mode global vars to use `gnat' for compiling."
  (add-to-list 'ada-prj-file-ext-extra     "gpr")
  (add-to-list 'ada-prj-parser-alist       '("gpr" . gnat-parse-gpr))
  (add-to-list 'ada-select-prj-compiler    '(gnat  . ada-gnat-compile-select-prj))
  (add-to-list 'ada-deselect-prj-compiler  '(gnat  . ada-gnat-compile-deselect-prj))

  (add-to-list 'ada-prj-parse-one-compiler   (cons 'gnat 'gnat-prj-parse-emacs-one))
  (add-to-list 'ada-prj-parse-final-compiler (cons 'gnat 'gnat-prj-parse-emacs-final))

  (font-lock-add-keywords 'ada-mode
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-preprocessor-face t))))

  (add-hook 'ada-gnat-fix-error-hook 'ada-gnat-fix-error))

(provide 'ada-gnat-compile)
(provide 'ada-compiler)

(ada-gnat-compile)

(add-to-list
 'compilation-error-regexp-alist-alist
 '(gnat
   ;; typical:
   ;;   cards_package.adb:45:32: expected private type "System.Address"
   ;;
   ;; with full path Source_Reference pragma :
   ;;   d:/maphds/version_x/1773/sbs-abi-dll_lib.ads.gp:39:06: file "interfaces_c.ads" not found
   ;;
   ;; gnu cc1: (gnatmake can invoke the C compiler)
   ;;   foo.c:2: `TRUE' undeclared here (not in a function)
   ;;   foo.c:2 : `TRUE' undeclared here (not in a function)
   "^\\(\\(.:\\)?[^ :\n]+\\):\\([0-9]+\\)\\s-?:?\\([0-9]+\\)?" 1 3 4))

(unless (default-value 'ada-compiler)
    (set-default 'ada-compiler 'gnat))

;; end of file
