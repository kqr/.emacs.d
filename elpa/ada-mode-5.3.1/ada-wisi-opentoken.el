;; ada-wisi-opentoken.el --- An indentation function for ada-wisi that indents  -*- lexical-binding:t -*-
;; OpenTokengrammar statements nicely.

;; Copyright (C) 2013-2017  Free Software Foundation, Inc.

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


;;; Commentary:

;; This is an example of a user-added indentation rule.
;;
;; In each file that declares OpenToken grammars, enable
;; ada-indent-opentoken minor mode by adding this near the end of the
;; file:
;;
;; Local Variables:
;; eval: (ada-indent-opentoken-mode)
;; End:

;;; Code:

(require 'ada-mode)
(require 'wisi)

(defun ada-wisi-opentoken ()
  "Return appropriate indentation (an integer column) for continuation lines in an OpenToken grammar statement."
  ;; We don't do any checking to see if we actually are in an
  ;; OpenToken grammar statement, since this rule should only be
  ;; included in package specs that exist solely to define OpenToken
  ;; grammar fragments.
  (save-excursion
    (let ((token-text (wisi-token-text (wisi-backward-token)))
	  cache)
      (cond
       ((equal token-text "<=")
	(+ (current-indentation) ada-indent-broken))

       ((member token-text '("+" "&"))
	(while (not (equal "<=" (wisi-token-text (wisi-backward-token)))))
	(+ (current-indentation) ada-indent-broken))

       ((equal token-text "and")
	;; test/ada_mode-opentoken.ads
	;; Tokens.Statement <= Add_Statement and
	;; Add_Statement <=
	;;   ... and
	;; Add_Statement <=
	;;
	(setq cache (wisi-goto-containing (wisi-backward-cache))) ;; Tokens.Statement
	(wisi-goto-containing cache);; :=

	(+ (current-indentation) ada-indent-broken))
       ))))

(defconst ada-wisi-opentoken-align
  '(ada-opentoken
    (regexp  . "[^=]\\(\\s-*\\)<=")
    (valid   . (lambda() (not (ada-in-comment-p))))
    (modes   . '(ada-mode)))
  "Align rule for OpenToken grammar definitions.")

;;;###autoload
(define-minor-mode ada-indent-opentoken-mode
  "Minor mode for indenting grammar definitions for the OpenToken package.
Enable mode if ARG is positive"
  :initial-value t
  :lighter       "OpenToken"   ;; mode line

  (if ada-indent-opentoken-mode
      (progn
	;; This must be after ada-wisi-setup on ada-mode-hook, because
	;; ada-wisi-setup resets wisi-indent-calculate-functions
	(add-to-list 'ada-align-rules ada-wisi-opentoken-align)
	(add-to-list 'wisi-indent-calculate-functions 'ada-wisi-opentoken))

    (setq ada-align-rules (delete ada-wisi-opentoken-align ada-align-rules))
    (setq wisi-indent-calculate-functions (delete 'ada-wisi-opentoken wisi-indent-calculate-functions))
    ))

(provide 'ada-wisi-opentoken)
;; end of file
