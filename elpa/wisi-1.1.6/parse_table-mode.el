;; parse_table-mode.el --- For navigating in a parse_table as output by wisi-generate. -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;; Version: 1.0
;; package-requires: ((emacs "25.1"))
;; URL: http://www.nongnu.org/ada-mode/wisi/wisi.html
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

(require 'xref)

(defun parse_table--xref-backend () 'parse_table)

(cl-defgeneric xref-backend-identifier-completion-table ((_backend (eql parse_table)))
  ;; could complete on nonterms, find productions
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql parse_table)))
  ;; assume we are on one of:
  ;; - ’goto state nnn’ in a state action
  ;; - ’=> State nnn’ in the debug kernels list
  ;; - ’( nnn)’ in the unknown conflicts list
  (save-excursion
    (end-of-line)
    (when (or (looking-back "[Ss]tate \\([0-9]+\\),?" (line-beginning-position))
	      (looking-back "( \\([0-9]+\\))" (line-beginning-position)))
      (match-string 1))))

(cl-defgeneric xref-backend-definitions ((_backend (eql parse_table)) identifier)
  ;; state tables are self-contained; IDENTIFIER must be a state number
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (concat "^State " identifier ":$"))
    (list (xref-make identifier (xref-make-buffer-location (current-buffer) (match-beginning 0))))))

(define-minor-mode parse_table-mode
  "Provides navigation in wisi-generate parse table output."
  nil ":parse_table" nil
  (add-hook 'xref-backend-functions #'parse_table--xref-backend nil t)

  (if parse_table-mode
      (read-only-mode 0)
    (read-only-mode 1)
  ))

(provide 'parse_table-mode)
;; end of file
