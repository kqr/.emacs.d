;;; ada-wisi.el --- Indentation engine for Ada mode, using the wisi generalized LALR parser  -*- lexical-binding:t -*-
;;
;; [1] ISO/IEC 8652:2012(E); Ada 2012 reference manual
;;
;; Copyright (C) 2012 - 2017  Free Software Foundation, Inc.
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
;;; History:
;;
;; implementation started Jan 2013
;;
;;;;

(require 'ada-fix-error)
(require 'ada-grammar-wy)
(require 'ada-indent-user-options)
(require 'cl-lib)
(require 'wisi)

(defconst ada-wisi-class-list
  '(
    block-end
    block-middle ;; not start of statement
    block-start ;; start of block is start of statement
    close-paren
    expression-start
    label
    list-break
    name
    name-paren ;; anything that looks like a procedure call, since the grammar can't distinguish most of them
    open-paren
    return
    return-with-params
    return-without-params
    statement-end
    statement-other
    statement-start
    ))

;;;; indentation

(defun ada-wisi-current-indentation (indenting)
  "Return indentation of current line, taking into account leading parens and labels.
INDENTING is point at indenting token."
  (let* ((parse-result (save-excursion (syntax-ppss indenting)))
	 (in-paren (ada-in-paren-p parse-result))
	 (cur-line (line-number-at-pos (point)))
	 (paren-pos (nth 1 parse-result))
	 (paren-line (line-number-at-pos paren-pos))
	 cache)
    (save-excursion
      (back-to-indentation)
      (cond
       ((and in-paren
	     (= cur-line paren-line))
	(goto-char paren-pos)
	(1+ (current-column)))

       ((and (setq cache (wisi-get-cache (point)))
	     (eq 'label (wisi-cache-class cache)))
	(- (current-column) ada-indent-label))

       (t
	(current-column))

       ))))

(defun ada-wisi-indent-containing (offset cache before indenting)
  "Return indentation of OFFSET plus indentation of token containing CACHE.
point should be at CACHE.
BEFORE should be t when called from ada-wisi-before-cache, nil otherwise.
If may be in parens, INDENTING must be point at indenting token."
  ;; First move to the right containing token
  (while
      (and (progn
	     (when (and (eq (wisi-cache-token cache) 'WHEN)
			(memq (wisi-cache-nonterm cache)
			      ;; not entry_body, exit_statement
			      '(case_statement_alternative
				;; test/ada_mode-nominal.adb
				;; case Local_4 is
				;;    when A =>
				case_expression_alternative
				;; test/ada_mode-conditional_expressions.adb
				;; elsif K = 0 then
				;;   (case J is
				;;       when 42                  => -1,
				exception_handler
				;; test/ada_mode-nominal.adb
				;; exception
				;;    when others =>
				;;       return 0.0;
				select_alternative
				;; test/ada_mode-nominal.adb
				;; or
				;;    when Started => -- Ada mode 4.01 ada-when-indent, GPS ada-indent
				variant
				;; test/generic_param.adb
				;; type Rte_Item_T (Item_Type : Item_Type_T := Fix) is record
				;;    case Item_Type is
				;;       when Fix | Airport =>
				)))
	       (setq offset (+ offset ada-indent-when)))
	     (setq cache (wisi-goto-containing cache nil)))
	   (or (memq
		(wisi-cache-token cache)
		'(EQUAL_GREATER
		  ;; test/ada_mode-nominal.adb
		  ;; case Local_4 is
		  ;;    when
		  ;;    ...
		  ;;      => -- Ada mode 4.01 indentation
		  ;;      ...
		  ;;    Loop_1 :
		  IS
		  ;; test/ada_mode-nominal.adb
		  ;; package body Ada_Mode.Nominal
		  ;; ...
		  ;; is
		  ;;    use Ada.Strings;
		  WHEN
		  ;; test/ada_mode-conditional_expressions.adb
		  ;; L3 : Integer := (case J is
		  ;;                     when 42 =>
		  ;;                        -1,
		  ))
	       (memq
		(wisi-cache-nonterm cache)
		'(name
		  ;; test/ada_mode-nominal.adb
		  ;; raise Constraint_Error with Count'Image (Line (File)) &
		  ;;    "foo";
		  ;;
		  ;; test/ada_mode-slices.adb
		  ;; Put_Line(Day'Image(D1) & " - " & Day'Image(D2) & " = " &
		  ;;            Integer'Image(N));
		  ))
	       (memq
		(wisi-cache-class cache)
		'(list-break
		  ;; test/ada_mode-nominal.adb
		  ;; 3
		  ;;   => (others => 3.0));
		  ))
	       (and (not (eq (current-column) (current-indentation)))
		    (memq (wisi-cache-class cache)
			  '(block-start
			    ;; test/ada_mode-nominal.adb
			    ;; or when Started
			    ;;   =>
			    block-middle
			    ;; test/ada_mode-quantified_expressions.adb
			    ;; for Element : Float of reverse
			    ;;   Board loop
			    ;;    Element := Element * 2.0;
			    ))
		    (not (eq (wisi-cache-token cache) 'RECORD))
		    ;; test/ada_mode-nominal.ads
		    ;; is tagged record
		    ;;    Component_1 : Integer; -- end 2
		    )
	       )))

  (cond
   (cache
    (cond
     ((memq (wisi-cache-class cache)
	    '(expression-start))
      ;; test/indent.ads
      ;; IO_CPU_Control_State_S_Pkg.CPU2_Fault_Status_Type'
      ;;   (Unused2  => 10,  -- Used to be aligned on "1 =>"
      ;;
      ;; test/ada_mode-conditional_expressions.adb
      ;; when B =>
      ;;   Fun (J) = 0
      ;;     or else M,
      ;;
      ;; test/hanging.adb
      ;; X_Long_Name
      ;;   := F_Long_Name
      ;;        (A => True,
      ;;         B => True);
      (if ada-indent-hanging-rel-exp
	  (+ (current-column) offset)
	(+ (ada-wisi-current-indentation indenting) offset)))

     (t
      ;; test/ada_mode-parens.adb
      ;; := Local_11_Type'(A => Integer
      ;;                     (1.0),
      ;;
      ;; test/ada_mode-parens.adb
      ;; or else ((B.all
      ;;             and then C)
      ;;            or else
      ;;
      ;; test/ada_mode-nominal.adb
      ;; => (others
      ;;       => 2.0),
      ;;
      ;; test/ada_mode-conditional_expressions.adb
      ;; K1 : Integer := (if J > 42 then -1
      ;;                  else +1);
      ;;
      ;; test/ada_mode-conditional_expressions.adb
      ;; K := (if K < 0 then 42
      ;;       elsif K = 0 then
      ;; test/ada_mode-nominal.adb
      ;; function Local_Function return Integer
      ;; is -- target 3
      (+ (ada-wisi-current-indentation indenting) offset))
     ))

   (t
    ;; at outermost containing statement. If called from
    ;; ada-wisi-before-cache, we want to ignore OFFSET (indenting
    ;; 'package' in a package spec). If called from
    ;; ada-wisi-after-cache, we want to include offset (indenting
    ;; first declaration in the package).
    (if before
	;; test/ada_mode-nominal.adb
	;; package body Ada_Mode.Nominal
	0
      ;; test/ada_mode-nominal.adb
      ;;      use Ada.Strings;
      offset)
    )))

(defun ada-wisi-indent-list-break (cache prev-token)
  "Return indentation for a token contained or preceeded by CACHE, which must be a list-break.
Point must be on CACHE. PREV-TOKEN is the token before the one being indented."
  (let ((break-point (point))
	(containing (wisi-goto-containing cache)))
    (cl-ecase (wisi-cache-token containing)
      (LEFT_PAREN
       (if (equal break-point (cadr prev-token))
	   ;; we are indenting the first token after the list-break; not hanging.
	   ;;
	   ;; test/parent.adb
	   ;; Append_To (Formals,
	   ;;            Make_Parameter_Specification (Loc,
	   ;;
	   ;; test/ada_mode-generic_instantiation.ads
	   ;; function Function_1 is new Instance.Generic_Function
	   ;;   (Param_Type  => Integer,
	   ;;    Result_Type => Boolean,
	   ;;
	   ;; test/ada_mode-parens.adb
	   ;; Local_14 : Local_14_Type :=
	   ;;   ("123",
	   ;;    "456" &
	   (+ (current-column) 1)

	 ;; else hanging
	 ;;
	 ;; test/ada_mode-parens.adb
	 ;; A :=
	 ;;   (1 |
	 ;;      2 => (1, 1, 1),
	 ;;    3 |
	 ;;      4 => (2, 2, 2));
	 (+ (current-column) 1 ada-indent-broken)))

      (IS
       ;; test/ada_mode-conditional_expressions.adb
       ;; L1 : Integer := (case J is
       ;;                     when 42 => -1,
       ;;                     -- comment aligned with 'when'
       (wisi-indent-paren (+ 1 ada-indent-when)))

      (WITH
       (cl-ecase (wisi-cache-nonterm containing)
	 (aggregate
	  ;; test/ada_mode-nominal-child.ads
	  ;; (Default_Parent with
	  ;;  Child_Element_1 => 10,
	  ;;  Child_Element_2 => 12.0,
	  (wisi-indent-paren 1))

	 (aspect_specification_opt
	  ;; test/aspects.ads:
	  ;; type Vector is tagged private
	  ;; with
	  ;;   Constant_Indexing => Constant_Reference,
	  ;;   Variable_Indexing => Reference,
	  (+ (current-indentation) ada-indent-broken))
	 ))
      )
    ))

(defun ada-wisi-before-cache ()
  "Point is at indentation, before a cached token. Return new indentation for point."
  (save-excursion
    (let ((start (point))
	  (cache (wisi-get-cache (point))))
      (when cache
	(cl-ecase (wisi-cache-class cache)
	  (block-start
	   (cl-case (wisi-cache-token cache)
	     (IS ;; subprogram body
	      (ada-wisi-indent-containing 0 cache t start))

	     (RECORD
	      ;; test/ada_mode-nominal.ads; ada-indent-record-rel-type = 3
	      ;; type Private_Type_2 is abstract tagged limited
	      ;;    record
	      ;;
	      ;; type Limited_Derived_Type_1d is
	      ;;   abstract limited new Private_Type_1 with
	      ;;    record
	      ;;
	      ;; for Record_Type_1 use
	      ;;   record
	      (let ((containing (wisi-goto-containing cache)))
		(while (not (memq (wisi-cache-token containing) '(FOR TYPE)))
		  (setq containing (wisi-goto-containing containing)))
		(+ (current-column) ada-indent-record-rel-type)))

	     (WHEN
	      ;; test/ada_mode-nominal.adb
	      ;; or
	      ;;    when Started => -- Ada mode 4.01 ada-when-indent, GPS ada-indent
	      (ada-wisi-indent-containing 0 cache t start))

	     (t ;; other
	      (ada-wisi-indent-containing ada-indent cache t start))))

	  (block-end
	   (cl-case (wisi-cache-nonterm cache)
	     (record_definition
	      (save-excursion
		(wisi-goto-containing cache);; now on 'record'
		(current-indentation)))

	     (t
	      (ada-wisi-indent-containing 0 cache t start))
	     ))

	  (block-middle
	   (ada-wisi-indent-containing 0 cache t start))

	  (close-paren
	   (wisi-indent-paren 0))

	  (expression-start
	   ;; defer to after-cache)
	   nil)

	  (label
	   ;; test/ada_mode-nominal.adb
	   ;;    <<Label_1>>
	   ;;
	   ;; Block_1:
	   ;;    declare -- label, no statements between begin, label
	   ;; indenting Block_1
	   (ada-wisi-indent-containing (+ ada-indent-label ada-indent) cache t start))

	  (list-break
	   ;; test/ada_mode-parens.adb
	   ;; Slice_1 (1
	   ;;        ,    --  used to get an error here; don't care about the actual indentation
	   ;;
	   ;; We don't actually care what the indentation is, since this
	   ;; should only occur while editing; defer to after-cache
	   ;; avoids an error and does something reasonable.
	   nil)

	  (name
	   (cond
	    ((let ((temp (wisi-get-containing-cache cache)))
	       (and temp
		    (memq (wisi-cache-nonterm temp) '(subprogram_body subprogram_declaration))))
	     ;; test/ada_mode-nominal.ads
	     ;; not
	     ;; overriding
	     ;; procedure
	     ;;   Procedure_1c (Item  : in out Parent_Type_1);
	     ;;
	     ;; not overriding function
	     ;;   Function_2e (Param : in Parent_Type_1) return Float;
	     (ada-wisi-indent-containing ada-indent-broken cache t start))

	    (t
	     ;; defer to ada-wisi-after-cache, for consistency
	     nil)
	    ))

	  (name-paren
	   ;; defer to ada-wisi-after-cache, for consistency
	   nil)

	  (open-paren
	   ;; test/ada_mode-nominal.adb
	   ;; entry E2
	   ;;   (X : Integer)
	   ;;
	   ;; test/ada_mode-conditional_expressions.adb
	   ;; K :=
	   ;;   (case Bounded (K) is
	   ;;
	   ;; test/ada_mode-conditional_expressions.adb
	   ;; when 1  =>
	   ;;   (if J > 42
	   ;;
	   (ada-wisi-indent-containing ada-indent-broken cache t start))

	  (return-with-params
	   ;; test/ada_mode-options-intent_return_1.ads, _2, _3
	   ;; indenting 'return' after ()
	   (let ((return-pos (point)))
	     (wisi-goto-containing cache nil) ;; matching 'function'
	     (cond
	      ((<= ada-indent-return 0)
	       ;; indent relative to "("
	       (wisi-forward-find-class 'open-paren return-pos)
	       (+ (current-column) (- ada-indent-return)))

	      (t
	       (+ (current-column) ada-indent-return))
	      )))

	  (return-without-params;; no parameter list
	   ;; test/ada_mode-generic_package.ads
	   ;; with function Concrete_Function_3
	   ;;   return Float is <>;
	   (wisi-goto-containing cache nil) ;; matching 'function'
	   (+ (current-indentation) ada-indent-broken))

	  (statement-end
	   (ada-wisi-indent-containing ada-indent-broken cache t start))

	  (statement-other
	   (cond
	    ((and (eq (wisi-cache-token cache) 'IS)
		  (memq (wisi-cache-nonterm cache)
			'(expression_function_declaration
			  ;; test/ada_mode-expression-functions.ads
			  ;; function Square (A : in Float) return Float
			  ;;   is (A * A);

			  null_procedure_declaration
			  ;; test/ada_mode-nominal.ads
			  ;; procedure Procedure_3c
			  ;; is null;
			  )))
	     (ada-wisi-indent-containing ada-indent-broken cache t start))

	    ((and (eq (wisi-cache-token cache) 'WITH)
		  (eq (wisi-cache-nonterm cache) 'derived_type_definition))
	     ;; test/ada_mode-nominal.ads
	     ;; type Limited_Derived_Type_2a is abstract limited new Private_Type_1
	     ;;   with record
	     (ada-wisi-indent-containing ada-indent-record-rel-type cache t start))

	    (t
	     (cl-case (wisi-cache-nonterm cache)
	       ((generic_renaming_declaration subprogram_renaming_declaration)
		;; special case for ada-indent-renames

		(cl-ecase (wisi-cache-token cache)
		  ((FUNCTION PROCEDURE)
		   ;; test/ada_mode-generic_instantiation.ads
		   ;; generic
		   ;; function Gen_Function_4 renames Instance.Generic_Function;
		   ;;
		   ;; generic
		   ;; procedure
		   (ada-wisi-indent-containing 0 cache t start))

		  (RENAMES
		   ;; test/ada_mode-generic_instantiation.ads
		   ;; generic function Gen_Function_2
		   ;;   renames Instance.Generic_Function;
		   (wisi-goto-containing cache)
		   (wisi-forward-find-token '(FUNCTION PROCEDURE) start)
		   (let ((pos-subprogram (point))
			 (has-params
			  ;; this is wrong for one return access
			  ;; function case: overriding function Foo
			  ;; return access Bar (...) renames ...;
			  (wisi-forward-find-token 'LEFT_PAREN start t)))
		     (if has-params
			 (if (<= ada-indent-renames 0)
			     ;; indent relative to paren
			     (+ (current-column) (- ada-indent-renames))
			   ;; else relative to line containing keyword
			   (goto-char pos-subprogram)
			   (+ (current-indentation) ada-indent-renames))

		       ;; no params
		       (goto-char pos-subprogram)
		       (+ (current-indentation) ada-indent-broken))
		     ))
		  ))

	       ((aggregate
		 ;; test/ada_mode-nominal-child.adb
		 ;; return (Parent_Type_1
		 ;;         with 1, 0.0, False);

		 aspect_specification_opt
		 ;; test/ada_mode-nominal.ads
		 ;; package Ada_Mode.Nominal
		 ;; with
		 ;;
		 ;; test/ada_mode-nominal.adb
		 ;; package body Ada_Mode.Nominal
		 ;; with

		 elsif_expression_item
		 ;; test/ada_mode-conditional_expressions.adb
		 ;; K := (if K < 0 then 42
		 ;;       elsif K = 0 then

		 elsif_statement_item
		 ;; not in paren
		 ;; test/g-comlin.adb
		 ;;   elsif Current_Argument < CL.Argument_Count then

		 generic_renaming_declaration
		 ;; test/ada_mode-generic_instantiation.ads
		 ;; generic
		 ;; procedure

		 subprogram_body
		 subprogram_declaration
		 subprogram_specification
		 overriding_indicator_opt
		 abstract_subprogram_declaration
		 expression_function_declaration
		 null_procedure_declaration
		 ;; test/ada_mode-nominal.ads
		 ;; not
		 ;; overriding
		 ;;
		 ;; not overriding
		 ;; procedure Procedure_1b
		 )
		(ada-wisi-indent-containing 0 cache t start))

	       (t
		(ada-wisi-indent-containing ada-indent-broken cache t start))
	       ))
	    )) ;; end statement-other

	  (statement-start
	   (ada-wisi-indent-containing ada-indent cache t start))
	  ))
      )))

(defun ada-wisi-after-cache ()
  "Point is at indentation. Find previous cached token, return
new indentation for point."
  (save-excursion
    (let ((start (point))
	  (prev-token (save-excursion (wisi-backward-token)))
	  (cache (wisi-backward-cache)))

      (cond
       ((not cache) ;; bob
	0)

       (t
	(while (memq (wisi-cache-class cache) '(close-paren name name-paren))
	  ;; not useful for indenting
	  (cond
	   ((eq (wisi-cache-class cache) 'close-paren)
	    (setq cache (wisi-goto-containing cache)))

	   (t
	    (setq cache (wisi-backward-cache)))))

	(cl-ecase (wisi-cache-class cache)
	  (block-end
	   ;; indenting block/subprogram name after 'end'
	   ;; test/ada_mode-nominal.adb
	   ;; end
	   ;;   Function_Access_11;
	   (wisi-indent-current ada-indent-broken))

	  (block-middle
	   (cl-case (wisi-cache-token cache)
	     (IS
	      (cl-case (wisi-cache-nonterm cache)
		(case_statement
		 ;; between 'case .. is' and first 'when'; most likely a comment
		 ;; test/ada_mode-nominal.adb
		 ;; case Param_1 is
		 ;; -- comment after "is", before "when"
		 (ada-wisi-indent-containing 0 cache nil start))

		(t
		 ;; test/ada_mode-nominal.adb
		 ;; function F2 (Param_1 : Discrete_Type_1; B : Float) return Float
		 ;; is
		 ;;    Local : Object_Access_Type_0a := new Float'(9.0);
		 (+ (ada-wisi-indent-containing ada-indent cache nil start)))
		))

	     ((THEN ELSE)
	      (let ((indent
		     (cl-ecase (wisi-cache-nonterm cache)
		       ((asynchronous_select
			;; test/ada_mode-nominal.adb
			 ;; select
			 ;;    delay 1.0;
			 ;; then
			 ;;    -- The comment after 'null' below has no space between ';'

			 if_statement
			 ;; test/ada_mode-nominal.adb
			 ;; if A_Param > 0.0 then
			 ;;    -- EMACSCMD:(test-face "new" font-lock-keyword-face)

			 elsif_statement_item
			 ;; test/ada_mode-nominal.adb
			 ;; then -- 2
			 ;;      --EMACSCMD:(progn ...

			 selective_accept
			 ;; test/ada_mode-nominal.adb
			 ;; else
			 ;;    --  comment after select else
			 )
			ada-indent)

		       ((if_expression
			;; test/ada_mode-conditional_expressions.adb
			;; K3 : Integer := (if
			;;                    J > 42
			;;                  then
			;;                    -1
			;;                  else
			;;                    +1);
			;; indenting -1, +1
			elsif_expression_item
			;; test/ada_mode-conditional_expressions.adb
			;; K := (if K < 0 then 42
			;;       elsif K = 0 then
			;;         (case J is
			)
			ada-indent-broken))))
		(ada-wisi-indent-containing indent cache nil start)))

	     (WHEN
	      (cl-ecase (wisi-cache-nonterm cache)
		((case_expression_alternative
		  ;; test/ada_mode-conditional_expressions.adb
		  ;; L2 : Integer := (case J is
		  ;;                     when
		  ;;                       42 => -1,

		  case_statement_alternative
		  ;; test/ada_mode-nominal.adb
		  ;; case Local_4 is
		  ;;    when
		  ;;      A | -- continuation line; ada-indent-broken = 2

		  exception_handler
		  ;; test/ada_mode-nominal.adb
		  ;; when -- 2
		  ;;   Bad_Thing -- ada-mode 4.01 indentation
		  ;;   =>        -- ""
		  )
		 (+ (current-column) ada-indent-broken))
		))

	     (t
	      ;; block-middle keyword other than above
	      ;; test/ada_mode-nominal.adb
	      ;; accept Middle_1 (Param_1 : in Integer) do
	      ;;    --  a comment after 'do'
	      ;;
	      ;; begin
	      ;;    --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 5"))
	      (ada-wisi-indent-containing ada-indent cache nil start))
	     ))

	  (block-start
	   (cl-case (wisi-cache-nonterm cache)
	     ((entry_body
	       ;; test/ada_mode-nominal.adb
	       ;; entry E2
	       ;;   (X : Integer)
	       ;;   -- an expression with 'not' to see if we need that in the

	       exception_handler
	       ;; test/ada_mode-nominal.adb
	       ;; when -- 2
	       ;;   Bad_Thing -- ada-mode 4.01 indentation

	       select_alternative
	       ;; test/ada_mode-nominal.adb
	       ;; when
	       ;;   Started =>
	       )
	       (+ (current-column) ada-indent-broken))

	     (t
	      ;; test/ada_mode-nominal.adb
	      ;; task body Task_Type_1 is
	      ;;    -- a more typical task
	      (+ (ada-wisi-current-indentation start) ada-indent))
	     ))

	  (expression-start
	   (cond
	    ((and (eq 'LEFT_PAREN (wisi-cache-token cache))
		  (not (ada-same-paren-depth-p start (point))))
	     (if (= (point) (cadr prev-token))
		 ;; test/ada_mode-parens.adb
		 ;; Local_9 : String := (
		 ;;                      "123" &
		 (1+ (current-column))

	       ;; not at token preceding indenting token
	       ;; test/ada_mode-nominal.adb
	       ;; 2
	       ;;   => (others
	       ;;         => 2.0),
	       ;;
	       ;; test/ada_mode-parens.adb
	       ;; A :=
	       ;;   (1 |
	       ;;      2 => (0, 0, 0),
	       (+ 1 (current-column) ada-indent-broken)))

	    (t
	     (let* ((containing1 (wisi-get-containing-cache cache))
		    (containing2 (wisi-get-containing-cache containing1))
		    (containing3 (wisi-get-containing-cache containing2)))

	       (cond
		((and (not (ada-pos-in-paren-p start))
		      (or (eq 'aspect_specification_opt (wisi-cache-nonterm containing2))
			  (eq 'aspect_specification_opt (wisi-cache-nonterm containing3))))
		 ;; special case for aspects
		 ;; test/aspects.ads
		 ;; with Pre => X > 10 and
                 ;;             X < 50 and
                 ;;             F (X),
		 ;;   Post =>
		 ;;     Y >= X and
		 ;;     Some_Very_Verbose_Predicate (X, Y);

		 ;;
		 ;; Indenting ’X < 50’
		 ;; cache nonterm is expression_opt at ’X < 10’
		 ;; containing1 nonterm is association_opt at ’=>’
		 ;; containing2 nonterm is aspect_specification_opt at ’with’
		 ;;
		 ;; Indenting ’F (X)’:
		 ;; cache nonterm is relation_and_list at ’X < 50’
		 ;; containing1 nonterm is expression_opt at ’X < 10’
		 ;; containing2 nonterm is association_opt at ’=>’
		 ;; containing3 nonterm is aspect_specification_opt at ’with’
		 ;;
		 ;; Indenting ’Y’
		 ;; cache nonterm is association_opt at ’=>’
		 ;; containing1 nonterm is association_list at ’,’
		 ;; containing2 nonterm is aspect_specification_opt at ’with’
		 ;;
		 ;; Indenting ’Some_...’:
		 ;; cache nonterm is expression_opt at ’Y’ (loop on ’name’ above)
		 ;; containing1 nonterm is association_opt at ’=>’
		 ;; containing2 nonterm is association_list at ’,’
		 ;; containing3 nonterm is aspect_specification_opt at ’with’
		 ;;
		 ;; does _not_ include this case:
		 ;; test/aspects.ads
		 ;; function Wuff return Boolean with Pre =>
		 ;;     (for all X in U =>
		 ;;        (if X in D then
		 ;; indenting ’(if’; handled by containing paren

		 ;; move point to ’=>’
		 (cond
		  ((eq 'aspect_specification_opt (wisi-cache-nonterm containing2))
		   (wisi-goto-containing cache))
		  ((eq 'aspect_specification_opt (wisi-cache-nonterm containing3))
		   (cond
		    ((eq 'association_opt (wisi-cache-nonterm containing2))
		     (wisi-goto-containing containing1))
		    ((eq 'association_list (wisi-cache-nonterm containing2))
		     (wisi-goto-containing cache))
		    ))
		  )

		 (let ((cache-col (current-column))
		       (cache-pos (point))
		       (line-end-pos (line-end-position)))
		   (if (= (+ 2 cache-pos) line-end-pos)
		       ;;   Post =>
		       ;;     Y >= X and
		       ;;     Some_Very_Verbose_Predicate (X, Y);
		       (progn
			 (goto-char cache-pos)
			 (+ (current-indentation) ada-indent-broken))
		     ;; with Pre => X > 10 and
		     ;;             X < 50 and
		     (+ 3 cache-col))
		   ))

		(t
		 ;; test/ada_mode-parens.adb
		 ;; if A.all
		 ;;   or else (B.all
		 ;;              --EMACSCMD:(test-face "then" 'font-lock-keyword-face)
		 ;;              and then C
		 ;;              and then D
		 ;; indenting last two lines
		 ;;
		 ;; test/hanging.adb
		 ;; if A
		 ;;      and then C
		 ;;      and then B
		 ;;                 /= A
		 ;;
		 ;; FIXME: we currently can't distinguish between 'and
		 ;; then C' and ' /= A'; the latter is indented wrong.
		 (while (memq (wisi-cache-nonterm cache)
			      '(relation_and_list
				relation_and_then_list
				relation_or_list
				relation_or_else_list
				relation_xor_list))
		   (setq cache (wisi-goto-containing cache)))

		 ;; test/ada_mode-parens.adb
		 ;; or else ((B.all
		 ;;             and then C)
		 ;;            or else
		 ;;
		 ;; test/ada_mode-parens.adb
		 ;; Local_2 : Integer := (1 + 2 +
		 ;;                         3);
		 ;;
		 ;; test/ada_mode-conditional_expressions.adb
		 ;; L5 : Boolean :=
		 ;;   (case C is
		 ;;       when A =>
		 ;;          J = 4
		 ;;            or else M, --  test case from Piotr Trojanek
		 ;;
		 ;; test/ada_mode-nominal.adb
		 ;; return 1.0 +
		 ;;   Function_2a (Parent_Type_1'(1, 2.0, False)) +
		 (if ada-indent-hanging-rel-exp
		     (+ (current-column) ada-indent-broken)
		   (+ (ada-wisi-current-indentation start) ada-indent-broken)))
		)))
	    ));; expression-start

	  (label
	   ;; test/ada_mode-nominal.adb
	   ;; <<Label_1>>
	   ;;    --  a comment after a label
	   (+ (current-column) (- ada-indent-label)))

	  (list-break
	   ;; test/ada_mode-nominal.adb
	   ;; with
	   ;;   Storage_Size => 512 + 256,
	   ;;   Priority => 5;
	   ;;
	   ;; (1 => (others => 1.0),
	   ;;  2
	   ;;
	   ;; test/format_paramlist.adb
	   ;; procedure X (Y : in     Z 'Class := Default_Z;
	   ;;              B : access Integer;
	   (ada-wisi-indent-list-break cache prev-token))

	  (open-paren
	   (cond
	    ((eq (cadr prev-token) (point))
	     ;; test/ada_mode-parens.adb
	     ;; return Float (
	     ;;               Integer'Value
	     (goto-char start)
	     (wisi-indent-paren 1))

	    (t
	     ;; test/ada_mode-parens.adb
	     ;; (Left,
	     ;;    Right : in Array_Type_1) -- ada-indent-broken to match 4.01
	     (goto-char start)
	     (wisi-indent-paren (1+ ada-indent-broken)))
	    ))

	  ((return-with-params
	    ;; test/ada_mode-nominal.adb
	    ;; function Function_Access_1
	    ;;   (A_Param : in Float)
	    ;;   return
	    ;;     Standard.Float
	    ;;
	    ;; test/ada_mode-nominal.ads
	    ;; function Function_2g
	    ;;   (Param : in Private_Type_1)
	    ;;   return Float
	    ;;     is abstract;
	    return-without-params
	    ;; test/ada_mode-nominal.adb
	    ;; type Function_Access_Type_2g is access protected function return
	    ;;   access Standard.Float;
	    )
	   (+ (current-indentation) ada-indent-broken))

	  (statement-end
	   ;; test/ada_mode-nominal.ads
	   ;; pragma Elaborate_Body (Ada_Mode.Nominal);
	   ;; -- Comment after one line of code; broken versions of the
	   (ada-wisi-indent-containing 0 cache nil start))

	  (statement-other
	   (cl-case (wisi-cache-token cache)
	     (ABORT
	      ;; test/subdir/ada_mode-separate_task_body.adb
	      ;; select
	      ;;    Please_Abort;
	      ;; then
	      ;;   abort
	      ;;    -- 'abort' indented with ada-indent-broken, since this is part
	      ;;    Titi;
	      (ada-wisi-indent-containing ada-indent cache nil start))

	     (COMMA
	      (cl-ecase (wisi-cache-nonterm cache)
		(name_list
		 (cl-ecase (wisi-cache-nonterm (wisi-get-containing-cache cache))
		   (use_clause
		    ;; test/with_use1.adb
		    ;; use Ada.Text_IO,
		    ;;     Ada.Numerics,   --  used to be indented with ada-broken-indent
		    (ada-wisi-indent-containing ada-indent-use cache nil start))

		   (with_clause
		    ;; test/ada_mode-nominal.ads
		    ;; limited private with Ada.Strings.Bounded,
		    ;;   --EMACSCMD:(test-face "Ada.Containers" 'default)
		    ;;   Ada.Containers;
		    ;;
		    ;; test/with_use1.adb
		    (ada-wisi-indent-containing ada-indent-with cache nil start))
		   ))
		))

	     (EQUAL_GREATER
	      (cond
	       ((or
		 (eq (wisi-cache-nonterm
		      (wisi-get-containing-cache cache))
		     'aspect_specification_opt)
		 ;; test/aspects.ads
		 ;; with
		 ;;   Pre =>
		 ;;     X > 10 and
		 (eq (wisi-cache-nonterm
		      (wisi-get-containing-cache (wisi-get-containing-cache cache)))
		     'aspect_specification_opt)
		 ;; test/aspects.ads
		 ;; with Pre => X > 10 and
		 ;;             X < 50 and
		 ;;             F (X),
		 ;;   Post =>
		 ;;     Y >= X and
		 )
		(ada-wisi-indent-containing (* 2 ada-indent-broken) cache nil start))

	       ((memq
		 (wisi-cache-nonterm cache)
		 '(case_statement_alternative
		   ;; test/ada_mode-nominal.adb
		   ;; when C =>
		   ;;    --EMACSCMD:...

		   case_expression_alternative
		   ;; test/ada_mode-conditional_expressions.adb
		   ;; L3 : Integer := (case J is
		   ;;                     when 42 =>
		   ;;                        -1,
		   ;;                     when Integer'First .. 41 =>
		   ;;                        0,
		   ;; indenting -1, 0

		   exception_handler
		   ;; test/ada_mode-nominal.adb
		   ;; when E : Constraint_Error =>

		   select_alternative
		   ;; test/test_select.adb
		   ;; or
		   ;;    when Local = 0 =>
		   ;;       --EMACSCMD:(test-face "E2" 'font-lock-function-name-face)
		   ))
		(ada-wisi-indent-containing ada-indent cache nil start))

	       (t
		;; test/ada_mode-parens.adb
		;; (1      =>
		;;    1,
		;;  2      =>
		;;    1 + 2 * 3,
		;; indenting '1,' or '1 +'
		(ada-wisi-indent-containing ada-indent-broken cache nil start))
	       ))

	     (IS
	      (cl-case (wisi-cache-nonterm cache)
		(full_type_declaration
		 ;; test/ada_mode-nominal.ads
		 ;; type Limited_Derived_Type_1a is abstract limited new
		 ;;    Private_Type_1 with record
		 (goto-char start)
		 (if (wisi-forward-find-token 'RECORD (line-end-position) t)
		     ;; 'record' on line being indented
		     (ada-wisi-indent-containing ada-indent-record-rel-type cache nil start)
		   ;; 'record' on later line
		   (ada-wisi-indent-containing ada-indent-broken cache nil start)))

		(t
		 ;; test/ada_mode-generic_package.ads
		 ;; type Synchronized_Formal_Derived_Type is abstract ...
		 ;;   with private;

		 ;; test/ada_mode-nominal.ads
		 ;;    subtype Subtype_2 is Signed_Integer_Type range 10 ..
		 ;;      20;
		 ;;
		 ;; type Private_Type_2 is abstract tagged limited
		 ;;  private;
		 ;;
		 ;; ada_mode-nominal.ads
		 ;; procedure Procedure_3b is
		 ;;   null;
		 (ada-wisi-indent-containing ada-indent-broken cache nil start))

		))

	     (WITH
	      (cl-ecase (wisi-cache-nonterm cache)
		(aggregate
		 ;; test/ada_mode-nominal-child.ads
		 ;;   (Default_Parent with
		 ;;    10, 12.0, True);
		 (wisi-indent-paren 1))

		(aspect_specification_opt
		 ;; test/aspects.ads
		 ;; type Vector is tagged private
		 ;; with
		 ;;   Constant_Indexing => Constant_Reference,
		 ;; indenting 'Constant_Indexing'; point is on 'with'
		 (+ (current-indentation) ada-indent-broken))

		(derived_type_definition
		 ;; test/ada_mode-nominal-child.ads
		 ;; type Child_Type_1 is new Parent_Type_1 with
		 ;;    -- comment between 'with' and 'record'
		 ;;    record
		 ;; indenting comment
		 (+ (current-indentation) ada-indent-record-rel-type))
		))

	     (t
	      ;; test/ada_mode-nominal.ads
	      ;; type Record_Type_3 (Discriminant_1 : access Integer) is tagged record
	      ;;    Component_1 : Integer; -- end 2
	      ;;    Component_2 :
	      ;;      Integer;
	      ;;
	      ;; test/ada_mode-nominal.adb
	      ;; Local_3 : constant Float :=
	      ;;   Local_2;
	      ;;
	      ;; test/g-comlin.adb
	      ;; elsif Index_Switches + Max_Length <= Switches'Last
	      ;;   and then Switches (Index_Switches + Max_Length) = '?'
	      ;;
	      ;; test/ada_mode-long_paren.adb
	      ;; Packet := new Packet_Type'
	      ;;   (RT                            => RT,
	      (+ (ada-wisi-current-indentation start) ada-indent-broken))
	     ))

	  (statement-start
	   (cl-case (wisi-cache-token cache)
	     (WITH
	      ;; test/ada_mode-nominal.ads
	      ;; with
	      ;;   Ada.Text_IO;
	      (+ (current-column) ada-indent-with))

	     (t
	      (if (ada-in-paren-p)
		  ;; test/ada_mode-conditional_expressions.adb
		  ;; K3 : Integer := (if
		  ;;                    J > 42
		  (wisi-indent-paren (1+ ada-indent-broken))

		;; not in paren
		;; test/ada_mode-generic_instantiation.ads
		;; procedure Procedure_8
		;;   is new Instance.Generic_Procedure (Integer, Function_1);
		(+ (ada-wisi-current-indentation start) ada-indent-broken)))
	     ))
	  )))
      )))

(defun ada-wisi-comment-gnat (indent after)
  "Modify INDENT to match gnat rules. Return new indent.
INDENT must be indent computed by `ada-wisi-after-cache'.
AFTER indicates what is on the previous line; one of:

code:         blank line, or code with no trailing comment
code-comment: code with trailing comment
comment:      comment"
  (let (prev-indent next-indent)
    ;; the gnat comment indent style check; comments must
    ;; be aligned to one of:
    ;;
    ;; - multiple of ada-indent
    ;; - next non-blank line
    ;; - previous non-blank line
    ;;
    ;; Note that we must indent the prev and next lines, in case
    ;; they are not currently correct.
    (cond
     ((and (not (eq after 'comment))
	   (= 0 (% indent ada-indent)))
      ;; this will handle comments at bob and eob, so we don't
      ;; need to worry about those positions in the next checks.
      indent)

     ((and (setq prev-indent
		 (if (eq after 'comment)
		     (progn (forward-comment -1) (current-column))
		   (save-excursion (forward-line -1)(indent-according-to-mode)(current-indentation))))
	   (= indent prev-indent))
      indent)

     ((and (setq next-indent
		 ;; we use forward-comment here, instead of
		 ;; forward-line, because consecutive comment
		 ;; lines are indented to the current one, which
		 ;; we don't know yet.
		 (save-excursion (forward-comment (point-max))(indent-according-to-mode)(current-indentation)))
	   (= indent next-indent))
      indent)

     (t
      (cl-ecase after
	(code-comment
	 ;; After comment that follows code on the same line
	 ;; test/ada_mode-conditional_expressions.adb
	 ;;
	 ;; then 44     -- comment matching GNAT
	 ;;             -- second line
	 ;;
	 ;; else 45)); -- comment _not_ matching GNAT style check
	 ;;             -- comment matching GNAT
	 ;;
	 (+ indent (- ada-indent (% indent ada-indent))))

	((code comment)
	 ;; After code with no trailing comment, or after comment
	 ;; test/ada_mode-conditional_expressions.adb
	 ;; (if J > 42
	 ;; -- comment indent matching GNAT style check
	 ;; -- second line of comment
	 prev-indent)

	))
     )))

(defun ada-wisi-comment ()
  "Compute indentation of a comment. For `wisi-indent-calculate-functions'."
  ;; We know we are at the first token on a line. We check for comment
  ;; syntax, not comment-start, to accomodate gnatprep, skeleton
  ;; placeholders, etc.
  (cond
   ((not (and (not (= (point) (point-max))) ;; no char after EOB!
	      (= 11 (syntax-class (syntax-after (point))))))
    ;; not looking at comment
    nil)

   ;; We are at a comment; indent to previous code or comment.
   ((and ada-indent-comment-col-0
	 (= 0 (current-column)))
    0)

   (t
    (let (after indent)
      (if (save-excursion (forward-line -1) (looking-at "\\s *$"))
	  ;; after blank line
	  (setq after 'code)

	(save-excursion
	  (forward-comment -1)
	  (if (looking-at "\\s *$")
	      ;; no comment on previous line
	      (setq after 'code)

	    (setq indent (current-column))
	    (if (not (= indent (progn (back-to-indentation) (current-column))))
		;; previous line has comment following code
		(setq after 'code-comment)
	      ;; previous line has plain comment
	      (setq indent (current-column))
	      (setq after 'comment)
	      )))
	)

      (cl-ecase after
	(code
	 ;; ada-wisi-before-cache will find the keyword _after_ the
	 ;; comment, which could be a block-middle or block-end, and that
	 ;; would align the comment with the block-middle, which is wrong. So
	 ;; we only call ada-wisi-after-cache.
	 (let ((indent (ada-wisi-after-cache)))
	   (if ada-indent-comment-gnat
	       (ada-wisi-comment-gnat indent 'code)
	     ;; not forcing gnat style
	     indent)))

	(comment
	 ;; assume previous line indented correctly
	 indent)

	(code-comment
	 (if ada-indent-comment-gnat
	     (ada-wisi-comment-gnat indent 'code-comment)
	   indent))
	)))
   ))

(defun ada-wisi-post-parse-fail ()
  "For `wisi-post-parse-fail-hook'."
  (save-excursion
    (let ((start-cache (wisi-goto-start (or (wisi-get-cache (point)) (wisi-backward-cache)))))
      (when start-cache
	;; nil when in a comment at point-min
	(indent-region (point) (wisi-cache-end start-cache)))
      ))
  (back-to-indentation))

;;;; ada-mode functions (alphabetical)

(defun ada-wisi-declarative-region-start-p (cache)
  "Return t if cache is a keyword starting a declarative region."
  (cl-case (wisi-cache-token cache)
   (DECLARE t)
   (IS
    (memq (wisi-cache-class cache) '(block-start block-middle)))
   (t nil)
   ))

(defun ada-wisi-context-clause ()
  "For `ada-fix-context-clause'."
  (wisi-validate-cache (point-max) t)
  (save-excursion
    (goto-char (point-min))
    (let ((begin nil)
	  (end nil)
	  cache)

      (while (not end)
	(setq cache (wisi-forward-cache))
	(cl-case (wisi-cache-nonterm cache)
	  (pragma (wisi-goto-end-1 cache))
	  (use_clause (wisi-goto-end-1 cache))
	  (with_clause
	   (when (not begin)
	     (setq begin (line-beginning-position)))
	   (wisi-goto-end-1 cache))
	  (t
	   ;; start of compilation unit
	   (setq end (line-beginning-position))
	   (unless begin
	     (setq begin end)))
	  ))
      (cons begin end)
    )))

(defun ada-wisi-on-context-clause ()
  "For `ada-on-context-clause'."
  (let (cache)
    (save-excursion
      ;; Don't require parse of large file just for ada-find-other-file
      (and (< (point-max) wisi-size-threshold)
	   (setq cache (wisi-goto-statement-start))
	   (memq (wisi-cache-nonterm cache) '(use_clause with_clause))
	   ))))

(defun ada-wisi-in-case-expression ()
  "For `ada-in-case-expression'."
  (save-excursion
    ;; Used by ada-align, which does indent, which will require parse
    ;; We know we are in a paren.
    (ada-goto-open-paren 1)
    (let ((cache (wisi-get-cache (point))))
      (and cache
	   (eq (wisi-cache-nonterm cache) 'case_expression)))
    ))

(defun ada-wisi-goto-subunit-name ()
  "For `ada-goto-subunit-name'."
  (wisi-validate-cache (point-max) t)

  (let ((end nil)
	cache
	(name-pos nil))
    (save-excursion
      ;; move to top declaration
      (goto-char (point-min))
      (setq cache (or (wisi-get-cache (point))
		      (wisi-forward-cache)))
      (while (not end)
	(cl-case (wisi-cache-nonterm cache)
	  ((pragma use_clause with_clause)
	   (wisi-goto-end-1 cache)
	   (setq cache (wisi-forward-cache)))
	  (t
	   ;; start of compilation unit
	   (setq end t))
	  ))
      (when (eq (wisi-cache-nonterm cache) 'subunit)
	(wisi-forward-find-class 'name (point-max)) ;; parent name
	(wisi-forward-token)
	(wisi-forward-find-class 'name (point-max)) ;; subunit name
	(setq name-pos (point)))
      )
    (when name-pos
      (goto-char name-pos))
    ))

(defun ada-wisi-goto-declaration-start (&optional include-type)
  "For `ada-goto-declaration-start', which see.
Also return cache at start."
  (wisi-validate-cache (point) t)

  (let ((cache (wisi-get-cache (point)))
	(done nil))
    (unless cache
      (setq cache (wisi-backward-cache)))
    ;; cache is null at bob
    (while (not done)
      (if cache
	  (progn
	    (setq done
		  (cl-case (wisi-cache-nonterm cache)
		    (full_type_declaration
		     (when include-type
		       (eq (wisi-cache-token cache) 'TYPE)))

		    ((generic_package_declaration generic_subprogram_declaration)
		     (eq (wisi-cache-token cache) 'GENERIC))

		    ((package_body package_declaration)
		     (eq (wisi-cache-token cache) 'PACKAGE))

		    ((protected_body protected_type_declaration single_protected_declaration)
		     (eq (wisi-cache-token cache) 'PROTECTED))

		    ((abstract_subprogram_declaration
		      subprogram_body
		      subprogram_declaration
		      subprogram_renaming_declaration
		      null_procedure_declaration)
		     (memq (wisi-cache-token cache) '(NOT OVERRIDING FUNCTION PROCEDURE)))

		    ((single_task_declaration task_body task_type_declaration)
		     (eq (wisi-cache-token cache) 'TASK))

		    ))
	    (unless done
	      (setq cache (wisi-goto-containing cache nil))))
	(setq done t))
	)
    cache))

(defun ada-wisi-goto-declaration-end ()
  "For `ada-goto-declaration-end', which see."
  ;; first goto-declaration-start, so we get the right end, not just
  ;; the current statement end.
  (wisi-goto-end-1 (ada-wisi-goto-declaration-start)))

(defun ada-wisi-goto-declarative-region-start ()
  "For `ada-goto-declarative-region-start', which see."
  (wisi-validate-cache (point) t)

  (let ((done nil)
	(first t)
	(cache
	 (or
	  (wisi-get-cache (point))
	  ;; we use forward-cache here, to handle the case where point is after a subprogram declaration:
	  ;; declare
	  ;;     ...
	  ;;     function ... is ... end;
	  ;;     <point>
	  ;;     function ... is ... end;
	  (wisi-forward-cache))))
    (while (not done)
      (if (ada-wisi-declarative-region-start-p cache)
	  (progn
	    (wisi-forward-token)
	    (setq done t))
	(cl-case (wisi-cache-class cache)
	  ((block-middle block-end)
	   (setq cache (wisi-prev-statement-cache cache)))

	  (statement-start
	   ;; 1) test/ada_mode-nominal.adb
	   ;;    protected body Protected_1 is -- target 2
	   ;;        <point>
	   ;;    want target 2
	   ;;
	   ;; 2) test/ada_mode-nominal.adb
	   ;;    function Function_Access_1
	   ;;      (A_Param <point> : in Float)
	   ;;      return
	   ;;        Standard.Float
	   ;;    is -- target 1
	   ;;    want target 1
	   ;;
	   ;; 3) test/ada_mode-nominal-child.adb
	   ;;    overriding <point> function Function_2c (Param : in Child_Type_1)
	   ;;                                    return Float
	   ;;    is -- target Function_2c
	   ;;    want target

	   (if first
	       ;; case 1
	       (setq cache (wisi-goto-containing cache t))
	     ;; case 2, 3
	     (cl-case (wisi-cache-nonterm cache)
	       (subprogram_body
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache))))
	       (t
		(setq cache (wisi-goto-containing cache t)))
	       )))
	  (t
	   (setq cache (wisi-goto-containing cache t)))
	  ))
      (when first (setq first nil)))
    ))

(defun ada-wisi-in-paramlist-p (&optional parse-result)
  "For `ada-in-paramlist-p'."
  (wisi-validate-cache (point))
  ;; (info "(elisp)Parser State" "*syntax-ppss*")
  (let ((parse-result (or parse-result (syntax-ppss)))
	 cache)
    (and (> (nth 0 parse-result) 0)
	 ;; cache is nil if the parse failed
	 (setq cache (wisi-get-cache (nth 1 parse-result)))
	 (eq 'formal_part (wisi-cache-nonterm cache)))
    ))

(defun ada-wisi-make-subprogram-body ()
  "For `ada-make-subprogram-body'."
  (wisi-validate-cache (point) t)

  (let* ((begin (point))
	 (end (save-excursion (wisi-forward-find-class 'statement-end (point-max)) (point)))
	 (cache (wisi-forward-find-class 'name end))
	 (name (buffer-substring-no-properties
		(point)
		(+ (point) (wisi-cache-last cache)))))
    (goto-char end)
    (newline)
    (insert " is begin\n\nend ");; legal syntax; parse does not fail
    (insert name)
    (forward-char 1)

    ;; newline after body to separate from next body
    (newline-and-indent)
    (indent-region begin (point))
    (forward-line -2)
    (back-to-indentation)
    ))

(defun ada-wisi-scan-paramlist (begin end)
  "For `ada-scan-paramlist'."
  (wisi-validate-cache end t)

  (goto-char begin)
  (let (token
	text
	identifiers
	(aliased-p nil)
	(in-p nil)
	(out-p nil)
	(not-null-p nil)
	(access-p nil)
	(constant-p nil)
	(protected-p nil)
	(type nil)
	type-begin
	type-end
	(default nil)
	(default-begin nil)
	param
	paramlist
	(done nil))
    (while (not done)
      (let ((token-text (wisi-forward-token)))
	(setq token (nth 0 token-text))
	(setq text  (wisi-token-text token-text)))
      (cond
       ((equal token 'COMMA) nil);; multiple identifiers

       ((equal token 'COLON)
	;; identifiers done. find type-begin; there may be no mode
	(skip-syntax-forward " ")
	(setq type-begin (point))
	(save-excursion
	  (while (member (car (wisi-forward-token)) '(ALIASED IN OUT NOT NULL ACCESS CONSTANT PROTECTED))
	    (skip-syntax-forward " ")
	    (setq type-begin (point)))))

       ((equal token 'ALIASED) (setq aliased-p t))
       ((equal token 'IN) (setq in-p t))
       ((equal token 'OUT) (setq out-p t))
       ((and (not type-end)
	     (member token '(NOT NULL)))
	;; "not", "null" could be part of the default expression
	(setq not-null-p t))
       ((equal token 'ACCESS) (setq access-p t))
       ((equal token 'CONSTANT) (setq constant-p t))
       ((equal token 'PROTECTED) (setq protected-p t))

       ((equal token 'COLON_EQUAL)
	(setq type-end (save-excursion (backward-char 2) (skip-syntax-backward " ") (point)))
	(skip-syntax-forward " ")
	(setq default-begin (point))
	(wisi-forward-find-token 'SEMICOLON end t))

       ((equal token 'LEFT_PAREN)
	;; anonymous access procedure type
	(goto-char (scan-sexps (1- (point)) 1)))

       ((member token '(SEMICOLON RIGHT_PAREN))
	(when (not type-end)
	  (setq type-end (save-excursion (backward-char 1) (skip-syntax-backward " ") (point))))

	(setq type (buffer-substring-no-properties type-begin type-end))

	(when default-begin
	  (setq default (buffer-substring-no-properties default-begin (1- (point)))))

	(when (equal token 'RIGHT_PAREN)
	  (setq done t))

	(setq param (list (reverse identifiers)
			  aliased-p in-p out-p not-null-p access-p constant-p protected-p
			  type default))
        (cl-pushnew param paramlist :test #'equal)
	(setq identifiers nil
	      aliased-p nil
	      in-p nil
	      out-p nil
	      not-null-p nil
	      access-p nil
	      constant-p nil
	      protected-p nil
	      type nil
	      type-begin nil
	      type-end nil
	      default nil
	      default-begin nil))

       (t
	(when (not type-begin)
          (cl-pushnew text identifiers :test #'equal)))
       ))
    paramlist))

(defun ada-wisi-which-function-1 (keyword add-body)
  "Used in `ada-wisi-which-function'."
  (let* ((cache (wisi-forward-find-class 'name (point-max)))
         (result (wisi-cache-text cache)))

    ;; See comment at ada-mode.el on why we don't overwrite ff-function-name.
    (when (not ff-function-name)
      (setq ff-function-name
	    (concat
	     keyword
	     (when add-body "\\s-+body")
	     "\\s-+"
	     result
	     ada-symbol-end)))
    result))

(defun ada-wisi-which-function (include-type)
  "For `ada-which-function'."
  (wisi-validate-cache (point))
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (> wisi-cache-max (point))
    (save-excursion
      (let ((result nil)
	    (cache (condition-case nil (ada-wisi-goto-declaration-start include-type) (error nil))))
	(if (null cache)
	    ;; bob or failed parse
	    (setq result "")

	  (when (memq (wisi-cache-nonterm cache)
		      '(generic_package_declaration generic_subprogram_declaration))
	    ;; name is after next statement keyword
	    (wisi-next-statement-cache cache)
	    (setq cache (wisi-get-cache (point))))

	  ;; add or delete 'body' as needed
	  (cl-ecase (wisi-cache-nonterm cache)
	    (full_type_declaration
	     (setq result (ada-wisi-which-function-1 "type" nil)))

	    (package_body
	     (setq result (ada-wisi-which-function-1 "package" nil)))

	    ((package_declaration
	      generic_package_declaration) ;; after 'generic'
	     (setq result (ada-wisi-which-function-1 "package" t)))

	    (protected_body
	     (setq result (ada-wisi-which-function-1 "protected" nil)))

	    ((protected_type_declaration single_protected_declaration)
	     (setq result (ada-wisi-which-function-1 "protected" t)))

	    ((abstract_subprogram_declaration
	      subprogram_declaration
	      generic_subprogram_declaration ;; after 'generic'
	      null_procedure_declaration)
	     (setq result (ada-wisi-which-function-1
			   (wisi-cache-text (wisi-forward-find-token '(FUNCTION PROCEDURE) (point-max)))
			   nil))) ;; no 'body' keyword in subprogram bodies

	    (subprogram_body
	     (setq result (ada-wisi-which-function-1
			   (wisi-cache-text (wisi-forward-find-token '(FUNCTION PROCEDURE) (point-max)))
			   nil)))

	    ((single_task_declaration task_type_declaration)
	     (setq result (ada-wisi-which-function-1 "task" t)))


	    (task_body
	     (setq result (ada-wisi-which-function-1 "task" nil)))
	    ))
	result))
    ))

;;;; debugging
(defun ada-wisi-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key ada-mode-map "\M-i" 'wisi-goto-statement-end)
  (define-key ada-mode-map "\M-j" 'wisi-show-cache)
  (define-key ada-mode-map "\M-k" 'wisi-show-token)
  )

(defun ada-wisi-number-p (token-text)
  "Return t if TOKEN-TEXT plus text after point matches the
syntax for a real literal; otherwise nil. point is after
TOKEN-TEXT; move point to just past token."
  ;; test in test/wisi/ada-number-literal.input
  ;;
  ;; starts with a simple integer
  (let ((end (point)))
    ;; this first test must be very fast; it is executed for every token
    (when (and (memq (aref token-text 0) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	       (string-match "^[0-9_]+$" token-text))
      (cond
       ((= (char-after) ?#)
	;; based number
	(forward-char 1)
	(if (not (looking-at "[0-9a-fA-F_]+"))
	    (progn (goto-char end) nil)

	  (goto-char (match-end 0))
	  (cond
	   ((= (char-after) ?#)
	    ;; based integer
	    (forward-char 1)
	    t)

	   ((= (char-after) ?.)
	    ;; based real?
	    (forward-char 1)
	    (if (not (looking-at "[0-9a-fA-F]+"))
		(progn (goto-char end) nil)

	      (goto-char (match-end 0))

	      (if (not (= (char-after) ?#))
		  (progn (goto-char end) nil)

		(forward-char 1)
		(setq end (point))

		(if (not (memq (char-after) '(?e ?E)))
		    ;; based real, no exponent
		    t

		  ;; exponent?
		  (forward-char 1)
		  (if (not (looking-at "[+-]?[0-9]+"))
		      (progn (goto-char end) t)

		    (goto-char (match-end 0))
		    t
		)))))

	   (t
	    ;; missing trailing #
	    (goto-char end) nil)
	   )))

       ((= (char-after) ?.)
	;; decimal real number?
	(forward-char 1)
	(if (not (looking-at "[0-9_]+"))
	    ;; decimal integer
	    (progn (goto-char end) t)

	  (setq end (goto-char (match-end 0)))

	  (if (not (memq (char-after) '(?e ?E)))
	      ;; decimal real, no exponent
	      t

	    ;; exponent?
	    (forward-char 1)
	    (if (not (looking-at "[+-]?[0-9]+"))
		(progn (goto-char end) t)

	      (goto-char (match-end 0))
	      t
	      ))))

       (t
	;; just an integer
	t)
       ))
    ))

(defun ada-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(ada-wisi-comment
		ada-wisi-before-cache
		ada-wisi-after-cache)
	      'ada-wisi-post-parse-fail
	      ada-wisi-class-list
	      ada-grammar-wy--keyword-table
	      ada-grammar-wy--token-table
	      ada-grammar-wy--parse-table)

  ;; Handle escaped quotes in strings
  (setq wisi-string-quote-escape-doubled t)

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)
  )

(add-hook 'ada-mode-hook 'ada-wisi-setup)

(setq ada-fix-context-clause 'ada-wisi-context-clause)
(setq ada-goto-declaration-end 'ada-wisi-goto-declaration-end)
(setq ada-goto-declaration-start 'ada-wisi-goto-declaration-start)
(setq ada-goto-declarative-region-start 'ada-wisi-goto-declarative-region-start)
(setq ada-goto-end 'wisi-goto-statement-end)
(setq ada-goto-subunit-name 'ada-wisi-goto-subunit-name)
(setq ada-in-case-expression 'ada-wisi-in-case-expression)
(setq ada-in-paramlist-p 'ada-wisi-in-paramlist-p)
(setq ada-indent-statement 'wisi-indent-statement)
(setq ada-make-subprogram-body 'ada-wisi-make-subprogram-body)
(setq ada-on-context-clause 'ada-wisi-on-context-clause)
(setq ada-reset-parser 'wisi-invalidate-cache)
(setq ada-scan-paramlist 'ada-wisi-scan-paramlist)
(setq ada-show-parse-error 'wisi-show-parse-error)
(setq ada-which-function 'ada-wisi-which-function)

(provide 'ada-wisi)
;; end of file
