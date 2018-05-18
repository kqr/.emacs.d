;; user options shared by Ada mode indentation engines  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012, 2013, 2015, 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Contributors: Simon Wright <simon.j.wright@mac.com>
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
;;; History: see ada_mode.el

;;;; code

(defgroup ada-indentation nil
  "Indentation options for Ada source."
  :group 'ada)

(defcustom ada-indent 3
  "Size of Ada default indentation, when no other indentation is used.

Example :
procedure Foo is
begin
>>>null;"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent)

(defvar ada-broken-indent nil)
(make-obsolete-variable
 'ada-broken-indent
 'ada-indent-broken
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-broken
  (if ada-broken-indent
      (progn
	(message "WARNING: setting `ada-indent-broken' to obsolete `ada-broken-indent'")
	ada-broken-indent)
    2)
  "Indentation for the continuation of a broken line.

Example :
   My_Var : My_Type :=
   >>(Field1 => Value);"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-broken)

(defcustom ada-indent-comment-col-0 nil
  "If non-nil, comments currently starting in column 0 are left in column 0.
Otherwise, they are indented with previous comments or code."
  :type 'boolean
  :safe #'booleanp)
(make-variable-buffer-local 'ada-indent-comment-col-0)

(defcustom ada-indent-comment-gnat nil
  "If non-nil, comments are indented to meet the GNAT comment style check.
That is, one of:

- multiple of ada-indent
- next non-blank line
- previous non-blank line

Otherwise, they are indented as a with previous comments or code."
  :type  'boolean
  :group 'ada-indentation
  :safe  #'booleanp)
(make-variable-buffer-local 'ada-indent-comment-gnat)

(defvar ada-label-indent nil)
(make-obsolete-variable
 'ada-label-indent
 'ada-indent-label
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-label
    (if ada-label-indent
      (progn
	(message "WARNING: setting `ada-indent-label' to obsolete `ada-label-indent'")
	ada-label-indent)
      -3)
  ;; Ada mode 4.01 and earlier default this to -4. But that is
  ;; incompatible with the default gnat indentation style check, which
  ;; wants all indentations to be a multiple of 3 (with some
  ;; exceptions). So we default this to -3.
  "Indentation for a loop, block, or statement label, relative to the item it labels.

Example :
   Label_1 :
   <<<<declare

   <<Label_2>>
   <<<<Foo := 0;"
  :type  'integer
  :safe  #'integerp)
(make-variable-buffer-local 'ada-indent-label)

(defcustom ada-indent-record-rel-type 3
  "Indentation for `record' relative to `type' or `use'.

An example is:
   type A is
   >>>record"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-record-rel-type)

(defcustom ada-indent-renames 2
  "Indentation for `renames' relative to the matching subprogram keyword.

For `renames' of non-subprograms the indentation is
`ada-indent-broken' relative to the line containing the matching
keyword.

If the subprogram has parameters then if `ada-indent-renames' is
zero or less the indentation is abs `ada-indent-renames' relative
to the open parenthesis; if `ada-indent-renames' is one or more
the indentation is relative to the line containing the keyword.

If the subprogram has no parameters then the indentation is
`ada-indent-broken' relative to the indentation of the line
containing the keyword.

Examples:
   ada-indent-renames = 2
   generic function A (B : Integer) return C
   >>renames Foo;

   ada-indent-renames = -1
   function A (B : Integer)
               return C
   >>>>>>>>>>>renames Foo;"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-renames)

(defcustom ada-indent-return 0
  "Indentation for `return' relative to the matching `function' keyword.

If the function has parameters, then if `ada-indent-return' is
zero or less the indentation is abs `ada-indent-return' relative
to the open parenthesis; if `ada-indent-return' is one or more,
indentation is relative to line containing `function'.

If the function has no parameters, `ada-indent-broken' is used
relative to line containing `function'.

An example is:
   function A (B : Integer)
   >>>>>>>>>>>return C;"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-return)

(defvar ada-use-indent nil)
(make-obsolete-variable
 'ada-use-indent
 'ada-indent-use
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-use
    (if ada-use-indent
      (progn
	(message "WARNING: setting `ada-indent-use' to obsolete `ada-use-indent'")
	ada-use-indent)
      ada-indent-broken)
  "Indentation for the lines in a `use' statement.

An example is:
   use Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-use)

(defvar ada-when-indent nil)
(make-obsolete-variable
 'ada-when-indent
 'ada-indent-when
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-when
    (if ada-when-indent
      (progn
	(message "WARNING: setting `ada-indent-when' to obsolete `ada-when-indent'")
	ada-when-indent)
      3)
  "Indentation for `when' relative to `exception', `case', `or' in select.

An example is:
   case A is
   >>>when B =>"
  :type  'integer
  :safe  #'integerp)
(make-variable-buffer-local 'ada-indent-when)

(defvar ada-with-indent nil)
(make-obsolete-variable
 'ada-with-indent
 'ada-indent-with
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-with
    (if ada-with-indent
      (progn
	(message "WARNING: setting `ada-indent-with' to obsolete `ada-with-indent'")
	ada-with-indent)
      ada-indent-broken)
  "Indentation for the lines in a `with' context clause.

An example is:
   with Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer
  :safe #'integerp)
(make-variable-buffer-local 'ada-indent-with)

(defcustom ada-indent-hanging-rel-exp nil
  "If non-nil, indent hanging lines relative to start of expression.
Otherwise, indent relative to previous line."
  :type 'boolean
  :safe #'booleanp)
(make-variable-buffer-local 'ada-indent-hanging-rel-exp)

(provide 'ada-indent-user-options)

;; end file
