;;; ada-imenu.el - Ada mode interface to imenu for Ada Mode  -*- lexical-binding:t -*-

;; Copyright (C) 2012, 2013, 2015  Free Software Foundation, Inc.
;;
;; Author: Simon Wright <simon@pushface.org>
;; Contributors: see ada-mode.el, and specifically Christian Egli
;;     <Christian.Egli@hcsd.hac.com> for ada-imenu-generic-expression
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
;;

(require 'ada-mode)
(require 'imenu)

(defconst ada--imenu-comment-re "\\([ \t]*--.*\\)?")

(defconst ada--imenu-subprogram-menu-re
  (concat "^[ \t]*\\(overriding[ \t]*\\)?\\(procedure\\|function\\)[ \t\n]+"
	  "\\(\\(\\sw\\|_\\)+\\)[ \t\n]*\\([ \t\n]\\|([^)]+)"
	  ada--imenu-comment-re
	  "\\)[ \t\n]*"
	  "\\(return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?is[ \t\n]"))

(defvar ada--imenu-generic-expression
  (list
   (list nil ada--imenu-subprogram-menu-re 3)
   (list "*Specs*"
	 (concat
	  "^[ \t]*\\(procedure\\|function\\)[ \t\n]+\\(\\(\\sw\\|_\\)+\\)"
	  "\\("
	  "\\(" ada--imenu-comment-re "[ \t\n]+\\|[ \t\n]*([^)]+)"
	  ada--imenu-comment-re "\\)";; parameter list or simple space
	  "\\([ \t\n]*return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?"
	  "\\)?;") 2)
   '("*Tasks*" "^[ \t]*task[ \t]+\\(type[ \t]+\\)?\\(\\(body[ \t]+\\)?\\(\\sw\\|_\\)+\\)" 2)
   '("*Type Defs*" "^[ \t]*\\(sub\\)?type[ \t]+\\(\\(\\sw\\|_\\)+\\)" 2)
   '("*Protected*"
     "^[ \t]*protected[ \t]+\\(type[ \t]+\\)?\\(\\(body[ \t]+\\)?\\(\\sw\\|_\\)+\\)" 2)
   '("*Packages*" "^[ \t]*package[ \t]+\\(\\(body[ \t]+\\)?\\(\\sw\\|[_.]\\)+\\)" 1))
  "Imenu generic expression for Ada mode.
See `imenu-generic-expression'.  This variable will create several submenus for
each type of entity that can be found in an Ada file.")

(defun ada--imenu-mode ()
  ;;  In 4.01, these were called in 'ada-mode or required to be set in
  ;;  the user's .emacs.

  (setq imenu-auto-rescan t)
  (setq imenu-case-fold-search t)
  (setq imenu-generic-expression ada--imenu-generic-expression)
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-use-markers nil)

  (imenu-add-to-menubar "Entities")
)

;; ada--imenu-mode does not depend on file local variables
(add-hook 'ada-mode-hook #'ada--imenu-mode)

(provide 'ada-imenu)

;;; ada-imenu.el ends here
