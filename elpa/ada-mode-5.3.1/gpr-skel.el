;; gpr-skel.el --- Extension to gpr-mode for inserting statement skeletons  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

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
;; The primary user command is `gpr-skel-expand', which inserts the
;; skeleton associated with the previous word (possibly skipping a
;; name).
;;
;; We don't define skeletons that prompt for most of the content; it
;; is easier just to type in the buffer.
;;
;; These skeletons are not intended to teach a novice the language,
;; just to make it easier to write code that the gpr-wisi parser
;; likes, and handle repeated names nicely.

;;; History:

;; Created Dec 2013

(require 'skeleton)
(require 'gpr-mode)

;;;;; user variables, example skeletons intended to be overwritten

(defgroup gpr nil
  "Major mode for editing GNAT project files in Emacs."
  :group 'languages)

(defcustom gpr-skel-initial-string "{header}\n{project}"
  "String to insert in empty buffer.
This could end in a token recognized by `gpr-skel-expand'."
  :type 'string
  :safe #'stringp)

(define-skeleton gpr-skel-user-restricted
  "Example copyright/license skeleton, with automatic year and owner."
  ()
  "--  Copyright (C) " (format-time-string "%Y ") user-full-name " All Rights Reserved.\n"
)

(define-skeleton gpr-skel-gpl
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
)

;;;;; Gpr skeletons (alphabetical)

(define-skeleton gpr-skel-case
  "Insert case statement."
  ()
  "case " str " is\n"
  "when " _ "=>\n"
  "end case;")

(define-skeleton gpr-skel-header
  "Insert a file header comment, with automatic copyright year and prompt for copyright owner/license.
Each user will probably want to override this."
  ()
  "--  Abstract :\n"
  "--\n"
  "--  " _ "\n"
  "--\n"
  "{copyright_license}\n"
  )

(define-skeleton gpr-skel-package
  "Insert a package with name from `str'."
  "Package name: "
  "package " str " is\n"
  _
  "end " str ";")

(define-skeleton gpr-skel-project
  "Insert a project with name from `str'."
  "Project name: "
  "project " str " is\n"
  _
  "end " str ";")

;;;;; skeleton extensions

;; FIXME: code below should be in skeleton.el

(defvar-local skeleton-token-alist nil
  "Symbol giving skeleton token alist of elements (STRING ELEMENT).
See `skeleton-expand'.
STRING must be a symbol in the current syntax, and is normally
the first keyword in the skeleton. All strings must be
lowercase; `skeleton-expand' converts user inputs.

ELEMENT may be:
- a skeleton, which is inserted
- an alist of (string . skeleton). User is prompted with `completing-read', selected skeleton is inserted. ")

(defun skeleton-add-skeleton (token skel alist &optional where)
  "Add an element (TOKEN . SKEL) to ALIST by side-effect.
If WHERE is nil, prepend to ALIST; otherwise, prepend to sublist
at WHERE."
  (if (null where)
      (setf alist (cons (cons token skel) alist))
    (setf (cdr (assoc where alist))
	  (cons (cons token skel) (cdr (assoc where alist))))
    ))

(defvar skeleton-test-input nil
  "When non-nil, bypasses prompt in alist token expansions - used for unit testing.")

(defun skeleton-build-prompt (alist count)
  "Build a prompt from the keys of the ALIST.
The prompt consists of the first COUNT keys from the alist, separated by `|', with
trailing `...' if there are more keys."
  (if (>= count (length alist))
      (concat (mapconcat 'car alist " | ") " : ")
    (let ((alist-1 (butlast alist (- (length alist) count))))
      (concat (mapconcat 'car alist-1 " | ") " | ... : "))
  ))

(defun skeleton-expand (&optional name)
  "Expand the token or placeholder before point to a skeleton, as defined by `skeleton-token-alist'.
A token is a symbol in the current syntax.
A placeholder is a symbol enclosed in generic comment delimiters.
If the word before point is not in `skeleton-token-alist', assume
it is a name, and use the word before that as the token."
  (interactive "*")

  ;; Skip trailing space, newline, and placeholder delimiter.
  ;; Standard comment end included for languages where that is newline.
  (skip-syntax-backward " !>")

  ;; include punctuation here, in case is is an identifier, to allow Gpr.Text_IO
  (let* ((end (prog1 (point) (skip-syntax-backward "w_.")))
	 (token (downcase (buffer-substring-no-properties (point) end)))
	 (skel (assoc-string token (symbol-value skeleton-token-alist)))
	 (handled nil))

    (if skel
	(progn
	  (when (listp (cdr skel))
	    (let* ((alist (cdr skel))
		   (prompt (skeleton-build-prompt alist 4)))
	      (setq skel (assoc-string
			  (or skeleton-test-input
			      (completing-read prompt alist))
			  alist))
	      (setq skeleton-test-input nil) ;; don't reuse input on recursive call
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
	;; FIXME: hook for Ada case adjust

	(setq token (buffer-substring-no-properties (point) end))

	(skeleton-expand token)
	(setq handled t)))

    (when (not handled)
      (error "undefined skeleton token: %s" name))
    ))

(defun skeleton-hippie-try (old)
  "For `hippie-expand-try-functions-list'. OLD is ignored."
  (if old
      ;; hippie is asking us to try the "next" completion; we don't have one
      nil
    (let ((pos (point))
	  (undo-len (if (sequencep pending-undo-list) (length pending-undo-list) 0)))
      (undo-boundary)
      (condition-case nil
	  (progn
	    (skeleton-expand)
	    t)
	(error
	 ;; undo hook action if any
	 (unless (= undo-len (if (sequencep pending-undo-list) (length pending-undo-list) 0))
	   (undo))

	 ;; undo motion
	 (goto-char pos)
	 nil)))))

(defun skeleton-next-placeholder ()
  "Move point forward to start of next placeholder."
  (interactive)
  (skip-syntax-forward "^!"))

(defun skeleton-prev-placeholder ()
  "Move point forward to start of next placeholder."
  (interactive)
  (skip-syntax-backward "^!"))

;; end FIXME:

;;;;; token alist, setup

(defconst gpr-skel-token-alist
  '(("case" . gpr-skel-case)
    ("copyright_license"
     ("GPL" . gpr-skel-gpl)
     ("restricted" . gpr-skel-user-restricted))
    ("header" . gpr-skel-header)
    ("package" . gpr-skel-package)
    ("project" . gpr-skel-project))
"For skeleton-token-alist")

(defun gpr-skel-setup ()
  "Setup a buffer gpr-skel."
  (setq skeleton-token-alist 'gpr-skel-token-alist)
  (add-hook 'skeleton-end-hook 'gpr-indent-statement nil t)
  (when (and gpr-skel-initial-string
	     (= (buffer-size) 0))
    (insert gpr-skel-initial-string))
  )

(provide 'gpr-skeletons)
(provide 'gpr-skel)

(setq gpr-expand #'skeleton-expand)

(add-hook 'gpr-mode-hook #'gpr-skel-setup)

;;; end of file
