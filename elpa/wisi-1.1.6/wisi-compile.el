;; wisi-compile.el --- Grammar compiler for the wisi parser, integrating Wisi OpenToken output.  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012-2013, 2015-2017 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

;;; Commentary:
;;

;;;; History: first experimental version Jan 2013
;;
;;;; Context
;;
;; Semantic (info "(semantic)Top") provides an LALR(1) parser
;; wisent-parse.  The grammar used is defined by the functions
;; semantic-grammar-create-package, which reads a bison-like source
;; file and produces corresponding elisp source, and
;; wisent-compile-grammar, which generates a parser table.
;;
;; However, the algorithm used in wisent-compile-grammar cannot cope
;; with the grammar for the Ada language, because it is not
;; LALR(1).  So we provide a generalized LALR parser, which spawns
;; parallel LALR parsers at each conflict.  Instead of also rewriting
;; the entire semantic grammar compiler, we use the OpenToken LALR
;; parser generator, which is easier to modify (it is written in Ada,
;; not Lisp).
;;
;; The Ada function Wisi.Generate reads the bison-like input and
;; produces corresponding elisp source code, similar to that
;; produced by semantic-grammar-create-package.
;;
;; wisi-compile-grammar (provided here) generates the automaton
;; structure required by wisi-parse
;;
;;;;

(defun wisi-compose-action (value symbol-obarray nonterms)
  (let* ((nonterm (car value))
	(index   (cdr value))
	(symbol (intern-soft (format "%s:%d" nonterm index) symbol-obarray))
	(rhs (car (nth index (cdr (assoc nonterm nonterms))))))
    (list nonterm symbol (length rhs))
    ))

(defun wisi-replace-actions (action symbol-obarray nonterms)
  "Replace semantic action symbol names in ACTION with list as defined in `wisi-compile-grammar'.
ACTION is the alist for one state from the grammar, with the form:
  ((default . error) ITEM ... )
ITEM is one of:
reduction  (TOKEN . (NONTERM . INDEX)) where NONTERM . INDEX gives the action symbol name.
shift (TOKEN . STATE)
shift/reduce conflict (STATE (NONTERM . INDEX))
reduce/shift conflict ((NONTERM . INDEX) (NONTERM . INDEX))

SYMBOL-OBARRAY contains the action symbols.
NONTERMS is from the grammar.
Return the new action alist."
  ;; result is list of (nonterm index action-symbol token-count)
  (let (result item)
    (while action
     (setq item (pop action))
     (cond
      ((or
	(memq (cdr item) '(error accept))
	(numberp (cdr item))) ;; shift
       (push item result))

      ((listp (cdr item))
       (let ((value (cdr item)))
	 (cond
	  ((symbolp (car value))
	   ;; reduction
	   (push (cons (car item)
		       (wisi-compose-action value symbol-obarray nonterms))
		 result))

	  ((integerp (car value))
	   ;; shift/reduce conflict
	   (push (cons (car item)
		       (list (car value)
			     (wisi-compose-action (cadr value) symbol-obarray nonterms)))
		 result))

	  (t ;; reduce/reduce conflict
	   (push (cons (car item)
		       (list (wisi-compose-action (car value) symbol-obarray nonterms)
			     (wisi-compose-action (cadr value) symbol-obarray nonterms)))
		 result))
	  )))

      (t
       (error "unexpected '%s'; expected 'error, 'accept, numberp, stringp, listp" (cdr item)))
      ));; while/cond

   (reverse result)))

(defun wisi-semantic-action (form nonterm iactn symbol-obarray)
  "Define an Elisp semantic action function for a production, interned in SYMBOL-OBARRAY.
FORM is the body of the semantic action.
NONTERM is the nonterminal left hand side.
IACTN is the index of the production in the NTERM rule.

The semantic action function accepts two arguments;
- $nterm      : the nonterminal
- wisi-tokens : the list of tokens to be reduced.

It returns nil; it is called for the semantic side-effects only."
  ;; based on comp.el wisent-semantic-action
  (let* ((name (format "%s:%d" nonterm iactn))
	 (action-symbol (intern name symbol-obarray)))

    (fset action-symbol
	  `(lambda ($nterm wisi-tokens)
	     ,form
	     nil))
    (byte-compile action-symbol)))

(defun wisi-compile-grammar (grammar)
  "Compile the LALR(1) GRAMMAR; return the automaton for wisi-parse.
GRAMMAR is a list TERMINALS NONTERMS ACTIONS GOTOS, where:

TERMINALS is a list of terminal token symbols.

NONTERMS is a list of productions; each production is a
list (nonterm (tokens semantic-action) ...) where `semantic-action' is
any lisp form. The set of (tokens semantic-action) are the right hand
sides; nonterm is the left hand side.

ACTIONS is an array indexed by parser state, of alists indexed by
terminal tokens. The value of each item in the alists is one of:

`error'

`accept'

integer - shift; gives new state

(nonterm . index) - reduce by nonterm production index.

(integer (nonterm . index)) - a shift/reduce conflict
((nonterm . index) (nonterm . index)) - a reduce/reduce conflict

The first item in the alist must have the key `default' (not a
terminal token); it is used when no other item matches the
current token.

GOTOS is an array indexed by parser state, of alists giving the
new state after a reduce for each nonterminal legal in that
state.

The automaton is an array [parser-actions gotos symbol-obarray]:

- parser-actions is a copy of the input ACTIONS, with semantic
actions replaced by a list (nonterm action-symbol token-count),
where:

-- nonterm is a symbol from NONTERMS, and is the non-terminal to
reduce to

-- token-count is the number of tokens in the reduction,

-- action-symbol is nil if there is no semantic action, or a
symbol interned in symbol-obarray

- gotos is a copy of GOTOS.

- symbol-obarray is an obarray containing functions that
implement the semantic action for each nonterminal; the function
names have the format nonterm:index."
  ;; We store named symbols for semantic actions, not just lambda
  ;; functions, so we have a name for debug trace.
  ;;
  ;; FIXME: TERMINALS is not used. Eliminating it requires decoupling
  ;; from OpenToken; we'll do that in the move to FastToken.
  ;;
  ;; FIXME: eliminate use of semantic-lex-* in *-wy.el. Similarly
  ;; requires decoupling from OpenToken

  (let ((defs (nth 1 grammar))
	(symbol-obarray (make-vector 13 0));; for parse actions
        (byte-compile-warnings '(not free-vars)) ;; for "wisi-test-success" in test/wisi/*
	def nonterm rhs-list rule
	semantic-action index)

    (while defs
      (setq def      (car defs)
            defs     (cdr defs)
            nonterm  (car def)
            rhs-list (cdr def)
            index    0)
      (while rhs-list
        (setq rule            (car rhs-list)
              rhs-list        (cdr rhs-list)
              semantic-action (cadr rule))

	(when semantic-action
	  (wisi-semantic-action semantic-action nonterm index symbol-obarray))

	(setq index (1+ index))
	))

    ;; replace semantic actions in ACTIONS with symbols from symbol-obarray
    (let ((nactions (length (nth 2 grammar)))
	  (actions (nth 2 grammar))
	  (i 0))
      (while (< i nactions)
	(aset actions i
	      (wisi-replace-actions (aref actions i) symbol-obarray (nth 1 grammar)))
	(setq i (1+ i)))
      (vector
       actions
       (nth 3 grammar)
       symbol-obarray)
      )))

(provide 'wisi-compile)
;;; wisi-compile.el ends here
