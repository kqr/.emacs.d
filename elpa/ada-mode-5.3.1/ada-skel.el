;;; ada-skel.el --- Extension to Ada mode for inserting statement skeletons  -*- lexical-binding:t -*-

;; Copyright (C) 1987, 1993, 1994, 1996-2017  Free Software Foundation, Inc.

;; Authors: Stephen Leake <stephen_leake@stephe-leake.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Design:
;;
;; The primary user command is `ada-skel-expand', which inserts the
;; skeleton associated with the previous word (possibly skipping a
;; name).
;;
;; We don't define skeletons that prompt for most of the content; it
;; is easier just to type in the buffer.
;;
;; These skeletons are not intended to teach a novice the language,
;; just to make it easier to write code that the ada-wisi parser
;; likes, and handle repeated names nicely.

;;; History:

;; Created May 1987.
;; Original version from V. Bowman as in ada.el of Emacs-18
;; (borrowed heavily from Mick Jordan's Modula-2 package for GNU,
;; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)
;;
;; Sep 1993. Daniel Pfeiffer <pfeiffer@cict.fr> (DP)
;; Introduced statement.el for smaller code and user configurability.
;;
;; Nov 1993. Rolf Ebert <ebert@enpc.fr> (RE) Moved the
;; skeleton generation into this separate file. The code still is
;; essentially written by DP
;;
;; Adapted Jun 1994. Markus Heritsch
;; <Markus.Heritsch@studbox.uni-stuttgart.de> (MH)
;; added menu bar support for templates
;;
;; 1994/12/02  Christian Egli <cegli@hcsd.hac.com>
;; General cleanup and bug fixes.
;;
;; 1995/12/20  John Hutchison <hutchiso@epi.syr.ge.com>
;; made it work with skeleton.el from Emacs-19.30. Several
;; enhancements and bug fixes.
;;
;; Sep 2013 Stephen Leake renamed to ada-skel (to match skeleton.el),
;; complete re-write: added ada-skel-alist (to get some of the
;; functionality of the sadly missed Else package). Simplified
;; skeletons; just make it easier to work with the ada-wisi parser,
;; don't try to teach syntax.

(require 'skeleton nil t)

;;;;; user variables, example skeletons intended to be overwritten

(defcustom ada-skel-initial-string
  "{header}
--  Emacs note: Type C-c C-e with point after the above placeholder
--
--  This text was inserted by ada-skel-initial-string;
--  M-x customize-variable <RET> ada-skel-initial-string <RET>
--  (info \"(ada-mode)Statement skeletons\")"
  "String to insert in empty buffer.
This could end in a token recognized by `ada-skel-expand'."
  :type 'string
  :group 'ada
  :safe #'stringp)

(define-skeleton ada-skel-user-restricted
  "Example copyright/license skeleton, with automatic year and owner."
  ()
  "--  Copyright (C) " (format-time-string "%Y ") user-full-name " All Rights Reserved.\n"
  "\n"
  "pragma License (Restricted);\n"
)

(define-skeleton ada-skel-gpl
  "Example copyright/license skeleton, with automatic year and owner, GPLv3."
  ()
  "--  Copyright (C) " (format-time-string "%Y ") user-full-name " All Rights Reserved.\n"
  "--\n"
  "--  This program is free software; you can redistribute it and/or\n"
  "--  modify it under terms of the GNU General Public License as\n"
  "--  published by the Free Software Foundation; either version 3, or (at\n"
  "--  your option) any later version. This program is distributed in the\n"
  "--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even\n"
  "--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR\n"
  "--  PURPOSE. See the GNU General Public License for more details. You\n"
  "--  should have received a copy of the GNU General Public License\n"
  "--  distributed with this program; see file COPYING. If not, write to\n"
  "--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,\n"
  "--  MA 02110-1335, USA.\n"
  "\n"
  "pragma License (GPL);\n"

)

(define-skeleton ada-skel-modified-gpl
  "Modified GPLv3 copyright/license skeleton, with automatic year and owner."
  ()
  "--  Copyright (C) " (format-time-string "%Y ") user-full-name " All Rights Reserved.\n"
  "--\n"
  "--  This library is free software;  you can redistribute it and/or modify it\n"
  "--  under terms of the  GNU General Public License  as published by the Free\n"
  "--  Software  Foundation;  either version 3,  or (at your  option) any later\n"
  "--  version. This library is distributed in the hope that it will be useful,\n"
  "--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-\n"
  "--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
  "\n"
  "--  As a special exception under Section 7 of GPL version 3, you are granted\n"
  "--  additional permissions described in the GCC Runtime Library Exception,\n"
  "--  version 3.1, as published by the Free Software Foundation.\n"
  "\n"
  "pragma License (Modified_GPL);\n"

)

;; override ada-mode 4.01 autoloaded functions
(define-obsolete-function-alias 'ada-header 'ada-skel-header "24.4"
  "Insert a descriptive header at the top of the file.")

;;;;; Ada skeletons (alphabetical)

(define-skeleton ada-skel-accept
  "Insert accept statement with name from `str'."
  ()
  "accept " str " do\n"
  "end " str ";")

(define-skeleton ada-skel-case
  "Insert case statement."
  ()
  "case " str " is\n"
  "when " _ "=>\n"
  "end case;")

(define-skeleton ada-skel-declare
  "Insert a block statement with an optional name (from `str')."
  ()
  str & ":\n"
  "declare\n"
  _
  "begin\n"
  "exception\n"
  "end " str | -1 ?\;)

(define-skeleton ada-skel-entry
  "Insert entry statement with name from `str'."
  ()
  "entry " str " when " _ "\n"
  "is\n"
  "begin\n"
  "end " str ";")

(define-skeleton ada-skel-for
  "Insert a for loop statement with an optional name (from `str')."
  ()
  str & " :\n"
  "for " _ " loop\n"
  "end loop " str | -1 ";")

(define-skeleton ada-skel-function-body
  "Insert a function body with name from `str'."
  ()
  "function " str " return \n"
  "is\n"
  "begin\n"
  _
  "end " str ";" >)

(define-skeleton ada-skel-function-spec
  "Insert a function type specification with name from `str'."
  ()
  "function " str " return ;")

(define-skeleton ada-skel-header
  "Insert a file header comment, with automatic copyright year and prompt for copyright owner/license.
Each user will probably want to override this."
  ()
  "--  Abstract :\n"
  "--\n"
  "--  " _ "\n"
  "--\n"
  "{copyright_license}\n"
  )

(define-skeleton ada-skel-if
  "Insert an if statement."
  ()
  "if " _ " then\n"
  "elsif  then\n"
  "else\n"
  "end if;")

(define-skeleton ada-skel-loop
  "Insert a loop statement with an optional name (from `str')."
  ()
  str & " :\n"
  "loop\n"
  "exit " str | -1 " when " _ ";\n"
  "end loop " str | -1 ";")

(define-skeleton ada-skel-package-body
  "Insert a package body with name from `str'."
  ()
  "package body " str " is\n"
  _
  "begin\n"
  "end " str ";")

(define-skeleton ada-skel-package-spec
  "Insert a package specification with name from `str'.
See `ada-find-other-file' to create library level package body from spec."
  ()
  "package " str " is\n"
  _
  "private\n"
  "end " str ";")

(define-skeleton ada-skel-procedure-body
  "Insert a procedure body with name from `str'."
  ()
  "procedure " str "\n"
  "is\n"
  "begin\n"
  _
  "end " str ";")

(define-skeleton ada-skel-procedure-spec
  "Insert a procedure type specification with name from `str'."
  ()
  "procedure " str ";")

(define-skeleton ada-skel-protected-body
  "Insert a protected body with name from `str'."
  ()
  "protected body " str " is\n"
  _
  "end " str ";")

(define-skeleton ada-skel-protected-spec
  "Insert a protected type specification with name from `str'."
  ()
  "protected type " str " is\n"
  _
  "private\n"
  "end " str ";")

(define-skeleton ada-skel-record
  "Insert a record type declaration with a type name from `str'."
  ()
  "type " str " is record\n"
  _
  "end record;")

(define-skeleton ada-skel-return
  "Insert an extended return statement."
  ()
  "return" _ "\n"
  "do\n"
  "end return;")

(define-skeleton ada-skel-select
  "Insert a select statement."
  ()
  "select\n"
  _
  "else\n"
  "end select;")

(define-skeleton ada-skel-task-body
  "Insert a task body with name from `str'."
  ()
  "task body " str "\n"
  "is\n"
  _
  "begin\n"
  "end " str ";")

(define-skeleton ada-skel-task-spec
  "Insert a task specification with name from `str'."
  ()
  "task " str " is\n"
  _
  "end " str ";")

(define-skeleton ada-skel-while
  "Insert a while loop statement with an optional name (from `str')."
  ()
  str & ":\n"
  "while " _ " loop\n"
  "end loop " str | -1 ";")

(define-skeleton ada-skel-with-use
  "Insert with and use context clauses with name from `str'."
  ()
  "with " str "; use " str ";")

;;;;; token alist, other functions

(defconst ada-skel-token-alist
  '(("accept" . ada-skel-accept)
    ("begin" . ada-skel-declare) ;; easy enough to delete the declare
    ("case" . ada-skel-case)
    ("copyright_license"
     ("GPL" . ada-skel-gpl)
     ("Modified GPL" . ada-skel-modified-gpl)
     ("restricted" . ada-skel-user-restricted))
    ("declare" . ada-skel-declare)
    ("entry" . ada-skel-entry)
    ("for" . ada-skel-for)
    ("function"
     ("body" . ada-skel-function-body)
     ("spec" . ada-skel-function-spec))
    ("header" . ada-skel-header)
    ("if" . ada-skel-if)
    ("loop" . ada-skel-loop)
    ("package"
     ("body" . ada-skel-package-body)
     ("spec" . ada-skel-package-spec))
    ("procedure"
     ("body" . ada-skel-procedure-body)
     ("spec" . ada-skel-procedure-spec))
    ("protected"
     ("body" . ada-skel-protected-body)
     ("spec" . ada-skel-protected-spec))
    ("record" . ada-skel-record)
    ("return" . ada-skel-return)
    ("select" . ada-skel-select)
    ("task"
     ("body" . ada-skel-task-body)
     ("spec" . ada-skel-task-spec))
    ("while" . ada-skel-while)
    ("with" . ada-skel-with-use))
  "alist of elements (STRING ELEMENT). See `ada-skel-expand'.
STRING must be a symbol in the current syntax, and is normally
the first Ada keyword in the skeleton. All strings must be
lowercase; `ada-skel-expand' converts user inputs.

ELEMENT may be:
- a skeleton, which is inserted
- an alist of (string . skeleton). User is prompted with `completing-read', selected skeleton is inserted. ")

(defvar ada-skel-test-input nil
  "When non-nil, bypasses prompt in alist token expansions - used for unit testing.")

(defun ada-skel-build-prompt (alist count)
  "Build a prompt from the keys of the ALIST.
The prompt consists of the first COUNT keys from the alist, separated by `|', with
trailing `...' if there are more keys."
  (if (>= count (length alist))
      (concat (mapconcat 'car alist " | ") " : ")
    (let ((alist-1 (butlast alist (- (length alist) count))))
      (concat (mapconcat 'car alist-1 " | ") " | ... : "))
  ))

(defun ada-skel-expand (&optional name)
  "Expand the token or placeholder before point to a skeleton, as defined by `ada-skel-token-alist'.
A token is a symbol in the current syntax.
A placeholder is a symbol enclosed in generic comment delimiters.
If the word before point is not in `ada-skel-token-alist', assume
it is a name, and use the word before that as the token."
  (interactive "*")

  ;; Skip trailing space, newline, and placeholder delimiter.
  ;; Standard comment end included for languages where that is newline.
  (skip-syntax-backward " !>")

  ;; include punctuation here, in case is is an identifier, to allow Ada.Text_IO
  (let* ((end (prog1 (point) (skip-syntax-backward "w_.")))
	 (token (downcase (buffer-substring-no-properties (point) end)))
	 (skel (assoc-string token ada-skel-token-alist))
	 (handled nil))

    (if skel
	(progn
	  (when (listp (cdr skel))
	    (let* ((alist (cdr skel))
		   (prompt (ada-skel-build-prompt alist 4)))
	      (setq skel (assoc-string
			  (or ada-skel-test-input
			      (completing-read prompt alist))
			  alist))
	      (setq ada-skel-test-input nil) ;; don't reuse input on recursive call
	      ))

	  ;; delete placeholder delimiters around token, token, and
	  ;; name. point is currently before token.
	  (skip-syntax-backward "!")
	  (delete-region
	   (point)
	   (progn
	     (skip-syntax-forward "!w_")
	     (when name
	       (skip-syntax-forward " ")
	       (skip-syntax-forward "w_."))
	     (point)))
	  (funcall (cdr skel) name)
	  (setq handled t))

      ;; word in point .. end is not a token; assume it is a name
      (when (not name)
	;; avoid infinite recursion

	;; Do this now, because skeleton insert won't.
	;;
	;; We didn't do it above, because we don't want to adjust case
	;; on tokens and placeholders.
	(save-excursion (ada-case-adjust-region (point) end))
	(setq token (buffer-substring-no-properties (point) end))

	(ada-skel-expand token)
	(setq handled t)))

    (when (not handled)
      (error "undefined skeleton token: %s" name))
    ))

(defun ada-skel-hippie-try (old)
  "For `hippie-expand-try-functions-list'. OLD is ignored."
  (if old
      ;; hippie is asking us to try the "next" completion; we don't have one
      nil
    (let ((pos (point))
	  (undo-len (if (eq 't pending-undo-list)
			0
		      (length pending-undo-list))))
      (undo-boundary)
      (condition-case nil
	  (progn
	    (ada-skel-expand)
	    t)
	(error
	 ;; undo hook action if any
	 (unless (or (eq 't pending-undo-list)
		     (= undo-len (length pending-undo-list)))
	   (undo))

	 ;; undo motion
	 (goto-char pos)
	 nil)))))

(defun ada-skel-next-placeholder ()
  "Move point to after next placeholder."
  (skip-syntax-forward "^!")
  (skip-syntax-forward "w!"))

(defun ada-skel-prev-placeholder ()
  "Move point to after previous placeholder."
  (skip-syntax-backward "^!"))

(defun ada-skel-setup ()
  "Setup a buffer for ada-skel."
  (add-hook 'skeleton-end-hook 'ada-indent-statement nil t)
  (when (and ada-skel-initial-string
	     (= (buffer-size) 0))
    (insert ada-skel-initial-string))
  )

(provide 'ada-skeletons)
(provide 'ada-skel)

(setq ada-expand #'ada-skel-expand)
(setq ada-next-placeholder #'ada-skel-next-placeholder)
(setq ada-prev-placeholder #'ada-skel-prev-placeholder)

(add-hook 'ada-mode-hook #'ada-skel-setup)

;;; ada-skel.el ends here
