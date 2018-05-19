;;; wisi-compat-24.2.el --- Implement current Emacs features not present in Emacs 24.2  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

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

;; using cl-lib 0.4 from Gnu ELPA

(when (not (boundp 'defvar-local))
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
	  (list 'make-variable-buffer-local (list 'quote var))))
  )

(provide 'wisi-compat-24.2)
;;; wisi-compat-24.2.el ends here
