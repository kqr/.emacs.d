;;; ada-gnat-xref.el --- Ada mode cross-reference functionality provided by 'gnat xref'  -*- lexical-binding:t -*-
;;
;; These tools are all Ada-specific; see gpr-query for multi-language
;; GNAT cross-reference tools.
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

(require 'ada-fix-error)
(require 'compile)
(require 'gnat-core)
(defvar ada-gnat-debug-run);; gnat-core requires ada-mode, which requires ada-gnat-xref

;;;;; code

;;;; uses of gnat tools

(defconst ada-gnat-file-line-col-regexp "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun ada-gnat-xref-adj-col (identifier col)
  "Return COL adjusted for 1-index, quoted operators."
  (cond
   ((eq ?\" (aref identifier 0))
    ;; There are two cases here:
    ;;
    ;; In both cases, gnat find wants the operators quoted, and the
    ;; column on the +. Gnat column is one-indexed; emacs is 0 indexed.
    ;;
    ;; In the first case, the front end passes in a column on the leading ", so we add one.
    ;;
    ;; In the second case, the front end passes in a column on the +
    (cond
     ((= ?\" (char-after (point)))
      ;; test/ada_mode-slices.adb
      ;; function "+" (Left : in Day; Right : in Integer) return Day;
      (+ 2 col))

     (t
      ;; test/ada_mode-slices.adb
      ;; D1, D2 : Day := +Sun;
      (+ 1 col))
     ))

   (t
    ;; Gnat column is one-indexed; emacs is 0 indexed.
    (+ 1 col))
   ))

(defun ada-gnat-xref-common-cmd ()
  "Returns the gnatfind command to run to find cross-references."
  (format "%sgnatfind" (or (ada-prj-get 'target) "")))

(defun ada-gnat-xref-common-args (identifier file line col)
  "Returns a list of arguments to pass to gnatfind.  The caller
may add more args to the result before calling gnatfind.  Some
elements of the result may be nil."
  (list "-a"
        (when ada-xref-full-path "-f")
	;; src_dir contains Source_Dirs from gpr_file, Similarly for
	;; obj_dir. So we don't need to pass the gpr file.
        (when (ada-prj-get 'src_dir)
          (concat "-aI" (mapconcat 'identity (ada-prj-get 'src_dir) ":")))
        (when (ada-prj-get 'obj_dir)
          (concat "-aO" (mapconcat 'identity (ada-prj-get 'obj_dir) ":")))
        (format "%s:%s:%d:%d"
                identifier
                (file-name-nondirectory file)
                line
                (ada-gnat-xref-adj-col identifier col))))

(defun ada-gnat-xref-other (identifier file line col)
  "For `ada-xref-other-function', using `gnatfind', which is Ada-specific."
  (let* ((result nil))
    (with-current-buffer (gnat-run-buffer)
      (gnat-run (ada-gnat-xref-common-cmd) (ada-gnat-xref-common-args identifier file line col))

      (goto-char (point-min))
      (when ada-gnat-debug-run (forward-line 2)); skip ADA_PROJECT_PATH, 'gnat find'

      ;; gnat find returns two items; the starting point, and the 'other' point
      (unless (looking-at (concat ada-gnat-file-line-col-regexp ":"))
	;; no results
	(error "'%s' not found in cross-reference files; recompile?" identifier))

      (while (not result)
	(looking-at (concat ada-gnat-file-line-col-regexp "\\(: warning:\\)?"))
	(if (match-string 4)
	    ;; error in *.gpr; ignore here.
	    (forward-line 1)
	  ;; else process line
	  (let ((found-file (match-string 1))
		(found-line (string-to-number (match-string 2)))
		(found-col  (string-to-number (match-string 3))))
	    (if (not
		 (and
		  ;; due to symbolic links, only the non-dir filename is comparable.
		  (equal (file-name-nondirectory file) (file-name-nondirectory found-file))
		  (= line found-line)
		  (= (ada-gnat-xref-adj-col identifier col) found-col)))
		;; found other item
		(setq result (list found-file found-line (1- found-col)))
	      (forward-line 1))
	    ))

	(when (eobp)
	  (error "gnat find did not return other item"))
	))
    result))

(defun ada-gnat-xref-parents (identifier file line col)
  "For `ada-xref-parents-function', using `gnatfind', which is Ada-specific."

  (let* ((arg (ada-gnat-xref-common-args identifier file line col))
	 (result nil))
    (with-current-buffer (gnat-run-buffer)
      (gnat-run (ada-gnat-xref-common-cmd) (cons "-d" arg))

      (goto-char (point-min))
      (when ada-gnat-debug-run (forward-line 2)); skip GPR_PROJECT_PATH, 'gnat find'

      ;; gnat find returns two items; the starting point, and the 'other' point
      (unless (looking-at (concat ada-gnat-file-line-col-regexp ":"))
	;; no results
	(error "'%s' not found in cross-reference files; recompile?" identifier))

      (while (not result)
	(looking-at (concat ada-gnat-file-line-col-regexp "\\(: warning:\\)?"))
	(if (match-string 4)
	    ;; error in *.gpr; ignore here.
	    (forward-line 1)
	  ;; else process line
	  (skip-syntax-forward "^ ")
	  (skip-syntax-forward " ")
	  (if (looking-at (concat "derived from .* (" ada-gnat-file-line-col-regexp ")"))
	      ;; found other item
	      (setq result (list (match-string 1)
				 (string-to-number (match-string 2))
				 (1- (string-to-number (match-string 3)))))
	    (forward-line 1))
	  )
	(when (eobp)
	  (error "gnat find did not return parent types"))
	))

    (ada-goto-source (nth 0 result)
		     (nth 1 result)
		     (nth 2 result)
		     nil ;; other-window
		     )
    ))

(defun ada-gnat-xref-all (identifier file line col local-only append)
  "For `ada-xref-all-function'."
  ;; we use `compilation-start' to run gnat, not `gnat-run', so it
  ;; is asynchronous, and automatically runs the compilation error
  ;; filter.

  (let* ((arg (ada-gnat-xref-common-args identifier file line col)))
    (setq arg (cons "-r" arg))
    (when local-only (setq arg (append arg (list file))))

    (with-current-buffer (gnat-run-buffer); for default-directory
      (let ((compilation-buffer-name "*gnatfind*")
            (compilation-error "reference")
            (command-and-args (mapconcat (lambda (a) (or a ""))
                                         (cons (ada-gnat-xref-common-cmd) arg)
                                         " "))
	    ;; gnat find uses standard gnu format for output, so don't
	    ;; need to set compilation-error-regexp-alist
	    prev-pos
	    prev-content)
	;; compilation-environment is buffer-local; don't set in 'let'
	(setq compilation-environment (ada-prj-get 'proc_env))

	;; WORKAROUND: the 'compilation' API doesn't let us specify "append", so we use this.
	(with-current-buffer (get-buffer-create compilation-buffer-name)
	  (when append
	    (setq prev-pos (point))
	    (setq prev-content (buffer-substring (point-min) (point-max))))

          (unless ada-gnat-debug-run
	    ;; hide the command and arguments using text properties, show only the bare minimum
	    (setq command-and-args
		  (propertize command-and-args
			      'display
			      (format "References to %s at %s:%d:%d" identifier file line col))))
	  (compilation-start command-and-args
			     'compilation-mode
			     (lambda (_name) compilation-buffer-name))
	  (when append
	    (let ((inhibit-read-only t))
		(goto-char (point-min))
		(insert prev-content)
		(goto-char prev-pos))))
	))))

;;;;; setup

(defun ada-gnat-xref-select-prj ()
  (setq ada-file-name-from-ada-name 'ada-gnat-file-name-from-ada-name)
  (setq ada-ada-name-from-file-name 'ada-gnat-ada-name-from-file-name)
  (setq ada-make-package-body       'ada-gnat-make-package-body)

  (setq ada-xref-other-function  'ada-gnat-xref-other)
  (setq ada-xref-parent-function 'ada-gnat-xref-parents)
  (setq ada-xref-all-function    'ada-gnat-xref-all)
  (setq ada-show-xref-tool-buffer 'ada-gnat-show-run-buffer)

  ;; gnatmake -gnatD generates files with .dg extensions. But we don't
  ;; need to navigate between them.

  (add-to-list 'completion-ignored-extensions ".ali") ;; gnat library files, used for cross reference
  (add-to-list 'compilation-error-regexp-alist 'gnat)
  )

(defun ada-gnat-xref-deselect-prj ()
  (setq ada-file-name-from-ada-name nil)
  (setq ada-ada-name-from-file-name nil)
  (setq ada-make-package-body       nil)

  (setq ada-xref-other-function  nil)
  (setq ada-xref-parent-function nil)
  (setq ada-xref-all-function    nil)
  (setq ada-show-xref-tool-buffer nil)

  (setq completion-ignored-extensions (delete ".ali" completion-ignored-extensions))
  (setq compilation-error-regexp-alist (delete 'gnat compilation-error-regexp-alist))
  )

(defun ada-gnat-xref ()
  "Set Ada mode global vars to use 'gnat xref'"
  (add-to-list 'ada-prj-file-ext-extra     "gpr")
  (add-to-list 'ada-prj-parser-alist       '("gpr" . gnat-parse-gpr))
  (add-to-list 'ada-select-prj-xref-tool   '(gnat  . ada-gnat-xref-select-prj))
  (add-to-list 'ada-deselect-prj-xref-tool '(gnat  . ada-gnat-xref-deselect-prj))

  ;; no parse-*-xref yet

  (add-hook 'ada-gnat-fix-error-hook 'ada-gnat-fix-error))

(ada-gnat-xref)

(provide 'ada-gnat-xref)

;; end of file
