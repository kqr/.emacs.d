;;; gpr-grammar-wy.el --- Generated parser support file  -*- lexical-binding:t -*-

;; Copyright (C) 2013 - 2015 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'wisi)
(require 'semantic/lex)
(require 'wisi-compile)

(defconst gpr-grammar-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(
    ("abstract" . ABSTRACT)
    ("aggregate" . AGGREGATE)
    ("case" . CASE)
    ("configuration" . CONFIGURATION)
    ("end" . END)
    ("extends" . EXTENDS)
    ("external" . EXTERNAL)
    ("external_as_list" . EXTERNAL_AS_LIST)
    ("for" . FOR)
    ("is" . IS)
    ("(" . LEFT_PAREN)
    ("library" . LIBRARY)
    ("null" . NULL)
    ("others" . OTHERS)
    ("package" . PACKAGE)
    ("project" . PROJECT)
    ("renames" . RENAMES)
    (")" . RIGHT_PAREN)
    ("standard" . STANDARD)
    ("type" . TYPE)
    ("use" . USE)
    ("when" . WHEN)
    ("with" . WITH)
    )
   nil)
  "Table of language keywords.")

(defconst gpr-grammar-wy--token-table
  (semantic-lex-make-type-table
   '(
     ("punctuation"
      (AMPERSAND . "&")
      (COLON . ":")
      (COLON_EQUALS . ":=")
      (COMMA . ",")
      (DOT . ".")
      (EQUAL_GREATER . "=>")
      (QUOTE . "'")
      (SEMICOLON . ";")
      (VERTICAL_BAR . "|")
     )
     ("symbol"
      (IDENTIFIER)
     )
     ("string-double"
      (STRING_LITERAL)
     )
    )
   nil)
  "Table of language tokens.")

(defconst gpr-grammar-wy--parse-table
   (wisi-compile-grammar
   '((AMPERSAND COLON COLON_EQUALS COMMA DOT EQUAL_GREATER QUOTE SEMICOLON VERTICAL_BAR IDENTIFIER STRING_LITERAL ABSTRACT AGGREGATE CASE CONFIGURATION END EXTENDS EXTERNAL EXTERNAL_AS_LIST FOR IS LEFT_PAREN LIBRARY NULL OTHERS PACKAGE PROJECT RENAMES RIGHT_PAREN STANDARD TYPE USE WHEN WITH )
     ((aggregate
       ((LEFT_PAREN string_list RIGHT_PAREN )
        (progn
        (wisi-statement-action [1 open-paren 3 close-paren])
        (wisi-containing-action 1 2))))
      (attribute_declaration
       ((FOR IDENTIFIER USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 statement-other 5 statement-end])
        (wisi-containing-action 3 4)))
       ((FOR IDENTIFIER LEFT_PAREN discrete_choice RIGHT_PAREN USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 open-paren 5 close-paren 6 statement-other 8 statement-end])
        (wisi-containing-action 6 7)))
       ((FOR EXTERNAL LEFT_PAREN STRING_LITERAL RIGHT_PAREN USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 open-paren 5 close-paren 6 statement-other 8 statement-end])
        (wisi-containing-action 6 7))))
      (attribute_prefix
       ((PROJECT ))
       ((name )))
      (attribute_reference
       ((attribute_prefix QUOTE IDENTIFIER ))
       ((attribute_prefix QUOTE IDENTIFIER LEFT_PAREN STRING_LITERAL RIGHT_PAREN )
        (wisi-statement-action [4 open-paren 6 close-paren])))
      (case_statement
       ((CASE name IS case_items END CASE SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 block-start 5 block-end 7 statement-end])
        (wisi-containing-action 3 4))))
      (case_item
       ((WHEN discrete_choice_list EQUAL_GREATER declarative_items_opt )
        (progn
        (wisi-statement-action [1 block-middle 3 block-start])
        (wisi-containing-action 1 3)
        (wisi-containing-action 3 4))))
      (case_items
       (())
       ((case_item ))
       ((case_items case_item )))
      (compilation_unit
       ((context_clause_opt project_qualifier_opt project_declaration_opt )))
      (context_clause
       ((with_clause ))
       ((context_clause with_clause )))
      (context_clause_opt
       (())
       ((context_clause )))
      (declarative_item
       ((simple_declarative_item ))
       ((typed_string_declaration ))
       ((package_declaration )))
      (declarative_items
       ((declarative_item ))
       ((declarative_items declarative_item )))
      (declarative_items_opt
       (())
       ((declarative_items )))
      (discrete_choice
       (())
       ((STRING_LITERAL ))
       ((OTHERS )))
      (discrete_choice_list
       ((discrete_choice ))
       ((discrete_choice_list VERTICAL_BAR discrete_choice )))
      (expression
       ((term ))
       ((expression AMPERSAND term )))
      (external_value
       ((EXTERNAL aggregate ))
       ((EXTERNAL_AS_LIST aggregate )))
      (identifier_opt
       (())
       ((IDENTIFIER )))
      (name
       ((identifier_opt ))
       ((name DOT IDENTIFIER )))
      (project_declaration_opt
       (())
       ((simple_project_declaration ))
       ((project_extension )))
      (package_declaration
       ((package_spec ))
       ((package_extension ))
       ((package_renaming )))
      (package_spec
       ((PACKAGE identifier_opt IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 block-start 5 block-end 7 statement-end])
        (wisi-containing-action 3 4))))
      (package_extension
       ((PACKAGE identifier_opt EXTENDS name IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 block-start 7 block-end 9 statement-end])
        (wisi-containing-action 5 6))))
      (package_renaming
       ((PACKAGE identifier_opt RENAMES name SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 statement-other 5 statement-end])
        (wisi-containing-action 3 4))))
      (project_extension
       ((PROJECT identifier_opt EXTENDS STRING_LITERAL IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 block-start 7 block-end 9 statement-end])
        (wisi-containing-action 5 6))))
      (project_qualifier_opt
       (())
       ((ABSTRACT ))
       ((STANDARD ))
       ((AGGREGATE ))
       ((AGGREGATE LIBRARY ))
       ((LIBRARY ))
       ((CONFIGURATION )))
      (simple_declarative_item
       ((IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 4 statement-end])
        (wisi-containing-action 1 3)))
       ((IDENTIFIER COLON IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 6 statement-end])
        (wisi-containing-action 1 5)))
       ((attribute_declaration ))
       ((case_statement ))
       ((NULL SEMICOLON )
        (wisi-statement-action [1 statement-start 2 statement-end])))
      (simple_project_declaration
       ((PROJECT identifier_opt IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 3 block-start 5 block-end 7 statement-end])
        (wisi-containing-action 3 4))))
      (string_primary
       ((STRING_LITERAL ))
       ((name ))
       ((external_value ))
       ((attribute_reference )))
      (string_list
       ((expression ))
       ((string_list COMMA expression )
        (progn
        (wisi-statement-action [2 list-break])
        (wisi-containing-action 2 3))))
      (term
       ((string_primary ))
       ((LEFT_PAREN RIGHT_PAREN ))
       ((aggregate )))
      (typed_string_declaration
       ((TYPE IDENTIFIER IS aggregate SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 statement-end])
        (wisi-containing-action 1 4))))
      (with_clause
       ((WITH string_list SEMICOLON ))))
     [((default . error) (ABSTRACT . (context_clause_opt . 0)) (AGGREGATE . (context_clause_opt . 0)) (CONFIGURATION . (context_clause_opt . 0)) (LIBRARY . (context_clause_opt . 0)) (STANDARD . (context_clause_opt . 0)) (PROJECT . (context_clause_opt . 0)) ($EOI . (context_clause_opt . 0)) (WITH .  7))
      ((default . error) ($EOI . (project_qualifier_opt . 1)) (PROJECT . (project_qualifier_opt . 1)))
      ((default . error) (LIBRARY .  34) ($EOI . (project_qualifier_opt . 3)) (PROJECT . (project_qualifier_opt . 3)))
      ((default . error) ($EOI . (project_qualifier_opt . 6)) (PROJECT . (project_qualifier_opt . 6)))
      ((default . error) ($EOI . (project_qualifier_opt . 5)) (PROJECT . (project_qualifier_opt . 5)))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) ($EOI . (project_qualifier_opt . 2)) (PROJECT . (project_qualifier_opt . 2)))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) ($EOI .  16))
      ((default . error) (ABSTRACT . (context_clause_opt . 1)) (AGGREGATE . (context_clause_opt . 1)) (CONFIGURATION . (context_clause_opt . 1)) (LIBRARY . (context_clause_opt . 1)) (STANDARD . (context_clause_opt . 1)) (PROJECT . (context_clause_opt . 1)) ($EOI . (context_clause_opt . 1)) (WITH .  7))
      ((default . error) (PROJECT . (project_qualifier_opt . 0)) ($EOI . (project_qualifier_opt . 0)) (ABSTRACT .  1) (STANDARD .  6) (AGGREGATE .  2) (LIBRARY .  4) (CONFIGURATION .  3))
      ((default . error) ($EOI . (project_declaration_opt . 2)))
      ((default . error) ($EOI . (project_declaration_opt . 1)))
      ((default . error) ($EOI . (context_clause . 0)) (PROJECT . (context_clause . 0)) (STANDARD . (context_clause . 0)) (LIBRARY . (context_clause . 0)) (CONFIGURATION . (context_clause . 0)) (AGGREGATE . (context_clause . 0)) (ABSTRACT . (context_clause . 0)) (WITH . (context_clause . 0)))
      ((default . error) ($EOI . (project_declaration_opt . 0)) (PROJECT .  5))
      ((default . error) (WITH . (context_clause . 1)) (ABSTRACT . (context_clause . 1)) (AGGREGATE . (context_clause . 1)) (CONFIGURATION . (context_clause . 1)) (LIBRARY . (context_clause . 1)) (STANDARD . (context_clause . 1)) (PROJECT . (context_clause . 1)) ($EOI . (context_clause . 1)))
      ((default . error) ($EOI . accept) (STRING_LITERAL . accept) (IDENTIFIER . accept) (VERTICAL_BAR . accept) (SEMICOLON . accept) (QUOTE . accept) (EQUAL_GREATER . accept) (DOT . accept) (COMMA . accept) (COLON_EQUALS . accept) (COLON . accept) (AMPERSAND . accept) (WITH . accept) (WHEN . accept) (USE . accept) (TYPE . accept) (STANDARD . accept) (RIGHT_PAREN . accept) (RENAMES . accept) (PROJECT . accept) (PACKAGE . accept) (OTHERS . accept) (NULL . accept) (LIBRARY . accept) (LEFT_PAREN . accept) (IS . accept) (FOR . accept) (EXTERNAL_AS_LIST . accept) (EXTERNAL . accept) (EXTENDS . accept) (END . accept) (CONFIGURATION . accept) (CASE . accept) (AGGREGATE . accept) (ABSTRACT . accept))
      ((default . error) (LEFT_PAREN .  44))
      ((default . error) (LEFT_PAREN .  44))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (RIGHT_PAREN . ( 42 (identifier_opt . 0))) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (QUOTE . (attribute_prefix . 0)))
      ((default . error) (COMMA . (identifier_opt . 1)) (RIGHT_PAREN . (identifier_opt . 1)) (EXTENDS . (identifier_opt . 1)) (RENAMES . (identifier_opt . 1)) (IS . (identifier_opt . 1)) (DOT . (identifier_opt . 1)) (AMPERSAND . (identifier_opt . 1)) (QUOTE . (identifier_opt . 1)) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (COMMA . (string_primary . 0)) (RIGHT_PAREN . (string_primary . 0)) (SEMICOLON . (string_primary . 0)) (AMPERSAND . (string_primary . 0)))
      ((default . error) (RIGHT_PAREN . (term . 2)) (COMMA . (term . 2)) (AMPERSAND . (term . 2)) (SEMICOLON . (term . 2)))
      ((default . error) (QUOTE .  41))
      ((default . error) (COMMA . (string_primary . 3)) (RIGHT_PAREN . (string_primary . 3)) (SEMICOLON . (string_primary . 3)) (AMPERSAND . (string_primary . 3)))
      ((default . error) (SEMICOLON . (string_list . 0)) (RIGHT_PAREN . (string_list . 0)) (COMMA . (string_list . 0)) (AMPERSAND .  40))
      ((default . error) (COMMA . (string_primary . 2)) (RIGHT_PAREN . (string_primary . 2)) (SEMICOLON . (string_primary . 2)) (AMPERSAND . (string_primary . 2)))
      ((default . error) (RIGHT_PAREN . (name . 0)) (COMMA . (name . 0)) (IS . (name . 0)) (AMPERSAND . (name . 0)) (SEMICOLON . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)))
      ((default . error) (COMMA . (string_primary . 1)) (RIGHT_PAREN . (string_primary . 1)) (SEMICOLON . (string_primary . 1)) (AMPERSAND . (string_primary . 1)) (DOT .  39) (QUOTE . (attribute_prefix . 1)))
      ((default . error) (RIGHT_PAREN . (term . 0)) (COMMA . (term . 0)) (AMPERSAND . (term . 0)) (SEMICOLON . (term . 0)))
      ((default . error) (COMMA .  37) (SEMICOLON .  38))
      ((default . error) (COMMA . (expression . 0)) (RIGHT_PAREN . (expression . 0)) (SEMICOLON . (expression . 0)) (AMPERSAND . (expression . 0)))
      ((default . error) (EXTENDS .  35) (IS .  36))
      ((default . error) ($EOI . (project_qualifier_opt . 4)) (PROJECT . (project_qualifier_opt . 4)))
      ((default . error) (STRING_LITERAL .  70))
      ((default . error) (END . (declarative_items_opt . 0)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (WITH . (with_clause . 0)) (ABSTRACT . (with_clause . 0)) (AGGREGATE . (with_clause . 0)) (CONFIGURATION . (with_clause . 0)) (LIBRARY . (with_clause . 0)) (STANDARD . (with_clause . 0)) (PROJECT . (with_clause . 0)) ($EOI . (with_clause . 0)))
      ((default . error) (IDENTIFIER .  51))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (RIGHT_PAREN . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (IDENTIFIER .  49))
      ((default . error) (SEMICOLON . (term . 1)) (AMPERSAND . (term . 1)) (COMMA . (term . 1)) (RIGHT_PAREN . (term . 1)))
      ((default . error) (COMMA .  37) (RIGHT_PAREN .  48))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (SEMICOLON . (external_value . 1)) (AMPERSAND . (external_value . 1)) (COMMA . (external_value . 1)) (RIGHT_PAREN . (external_value . 1)))
      ((default . error) (SEMICOLON . (external_value . 0)) (AMPERSAND . (external_value . 0)) (COMMA . (external_value . 0)) (RIGHT_PAREN . (external_value . 0)))
      ((default . error) ($EOI . (compilation_unit . 0)))
      ((default . error) (COMMA . (aggregate . 0)) (RIGHT_PAREN . (aggregate . 0)) (SEMICOLON . (aggregate . 0)) (AMPERSAND . (aggregate . 0)))
      ((default . error) (LEFT_PAREN .  82) (RIGHT_PAREN . (attribute_reference . 0)) (COMMA . (attribute_reference . 0)) (AMPERSAND . (attribute_reference . 0)) (SEMICOLON . (attribute_reference . 0)))
      ((default . error) (SEMICOLON . (expression . 1)) (COMMA . (expression . 1)) (RIGHT_PAREN . (expression . 1)) (AMPERSAND . (expression . 1)))
      ((default . error) (IS . (name . 1)) (RIGHT_PAREN . (name . 1)) (COMMA . (name . 1)) (AMPERSAND . (name . 1)) (SEMICOLON . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)))
      ((default . error) (AMPERSAND .  40) (RIGHT_PAREN . (string_list . 1)) (SEMICOLON . (string_list . 1)) (COMMA . (string_list . 1)))
      ((default . error) (DOT . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (EXTERNAL .  79) (IDENTIFIER .  80))
      ((default . error) (SEMICOLON .  78))
      ((default . error) (IS . (identifier_opt . 0)) (EXTENDS . (identifier_opt . 0)) (RENAMES . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (IDENTIFIER .  76))
      ((default . error) (COLON .  74) (COLON_EQUALS .  75))
      ((default . error) (WHEN . (simple_declarative_item . 2)) (END . (simple_declarative_item . 2)) (CASE . (simple_declarative_item . 2)) (FOR . (simple_declarative_item . 2)) (NULL . (simple_declarative_item . 2)) (PACKAGE . (simple_declarative_item . 2)) (TYPE . (simple_declarative_item . 2)) (IDENTIFIER . (simple_declarative_item . 2)))
      ((default . error) (WHEN . (simple_declarative_item . 3)) (END . (simple_declarative_item . 3)) (CASE . (simple_declarative_item . 3)) (FOR . (simple_declarative_item . 3)) (NULL . (simple_declarative_item . 3)) (PACKAGE . (simple_declarative_item . 3)) (TYPE . (simple_declarative_item . 3)) (IDENTIFIER . (simple_declarative_item . 3)))
      ((default . error) (WHEN . (declarative_items . 0)) (END . (declarative_items . 0)) (CASE . (declarative_items . 0)) (FOR . (declarative_items . 0)) (NULL . (declarative_items . 0)) (PACKAGE . (declarative_items . 0)) (TYPE . (declarative_items . 0)) (IDENTIFIER . (declarative_items . 0)))
      ((default . error) (WHEN . (declarative_items_opt . 1)) (END . (declarative_items_opt . 1)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (END .  72))
      ((default . error) (WHEN . (declarative_item . 2)) (END . (declarative_item . 2)) (IDENTIFIER . (declarative_item . 2)) (TYPE . (declarative_item . 2)) (PACKAGE . (declarative_item . 2)) (NULL . (declarative_item . 2)) (FOR . (declarative_item . 2)) (CASE . (declarative_item . 2)))
      ((default . error) (WHEN . (package_declaration . 0)) (END . (package_declaration . 0)) (CASE . (package_declaration . 0)) (FOR . (package_declaration . 0)) (NULL . (package_declaration . 0)) (PACKAGE . (package_declaration . 0)) (TYPE . (package_declaration . 0)) (IDENTIFIER . (package_declaration . 0)))
      ((default . error) (WHEN . (package_declaration . 1)) (END . (package_declaration . 1)) (CASE . (package_declaration . 1)) (FOR . (package_declaration . 1)) (NULL . (package_declaration . 1)) (PACKAGE . (package_declaration . 1)) (TYPE . (package_declaration . 1)) (IDENTIFIER . (package_declaration . 1)))
      ((default . error) (WHEN . (package_declaration . 2)) (END . (package_declaration . 2)) (CASE . (package_declaration . 2)) (FOR . (package_declaration . 2)) (NULL . (package_declaration . 2)) (PACKAGE . (package_declaration . 2)) (TYPE . (package_declaration . 2)) (IDENTIFIER . (package_declaration . 2)))
      ((default . error) (WHEN . (declarative_item . 0)) (END . (declarative_item . 0)) (IDENTIFIER . (declarative_item . 0)) (TYPE . (declarative_item . 0)) (PACKAGE . (declarative_item . 0)) (NULL . (declarative_item . 0)) (FOR . (declarative_item . 0)) (CASE . (declarative_item . 0)))
      ((default . error) (WHEN . (declarative_item . 1)) (END . (declarative_item . 1)) (IDENTIFIER . (declarative_item . 1)) (TYPE . (declarative_item . 1)) (PACKAGE . (declarative_item . 1)) (NULL . (declarative_item . 1)) (FOR . (declarative_item . 1)) (CASE . (declarative_item . 1)))
      ((default . error) (IS .  71))
      ((default . error) (END . (declarative_items_opt . 0)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (WHEN . (declarative_items . 1)) (IDENTIFIER . (declarative_items . 1)) (TYPE . (declarative_items . 1)) (PACKAGE . (declarative_items . 1)) (NULL . (declarative_items . 1)) (FOR . (declarative_items . 1)) (CASE . (declarative_items . 1)) (END . (declarative_items . 1)))
      ((default . error) (IDENTIFIER .  93))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (IS .  91))
      ((default . error) (IS .  89) (EXTENDS .  88) (RENAMES .  90))
      ((default . error) (WHEN . (simple_declarative_item . 4)) (IDENTIFIER . (simple_declarative_item . 4)) (TYPE . (simple_declarative_item . 4)) (PACKAGE . (simple_declarative_item . 4)) (NULL . (simple_declarative_item . 4)) (FOR . (simple_declarative_item . 4)) (CASE . (simple_declarative_item . 4)) (END . (simple_declarative_item . 4)))
      ((default . error) (LEFT_PAREN .  87))
      ((default . error) (USE .  86) (LEFT_PAREN .  85))
      ((default . error) (DOT .  39) (IS .  84))
      ((default . error) (STRING_LITERAL .  83))
      ((default . error) (RIGHT_PAREN .  112))
      ((default . error) (END . (case_items . 0)) (WHEN . ( 109 (case_items . 0))))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 0)) (STRING_LITERAL .  107) (OTHERS .  106))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (STRING_LITERAL .  104))
      ((default . error) (DOT . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (END . (declarative_items_opt . 0)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (LEFT_PAREN .  44))
      ((default . error) (AMPERSAND .  40) (SEMICOLON .  99))
      ((default . error) (COLON_EQUALS .  98))
      ((default . error) (SEMICOLON .  97))
      ((default . error) (END .  96))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) ($EOI . (simple_project_declaration . 0)))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (WHEN . (simple_declarative_item . 0)) (IDENTIFIER . (simple_declarative_item . 0)) (TYPE . (simple_declarative_item . 0)) (PACKAGE . (simple_declarative_item . 0)) (NULL . (simple_declarative_item . 0)) (FOR . (simple_declarative_item . 0)) (CASE . (simple_declarative_item . 0)) (END . (simple_declarative_item . 0)))
      ((default . error) (SEMICOLON .  123))
      ((default . error) (DOT .  39) (SEMICOLON .  122))
      ((default . error) (END .  121))
      ((default . error) (DOT .  39) (IS .  120))
      ((default . error) (RIGHT_PAREN .  119))
      ((default . error) (AMPERSAND .  40) (SEMICOLON .  118))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 2)) (VERTICAL_BAR . (discrete_choice . 2)) (EQUAL_GREATER . (discrete_choice . 2)))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 1)) (VERTICAL_BAR . (discrete_choice . 1)) (EQUAL_GREATER . (discrete_choice . 1)))
      ((default . error) (RIGHT_PAREN .  117))
      ((default . error) (VERTICAL_BAR . (discrete_choice . 0)) (EQUAL_GREATER . (discrete_choice . 0)) (STRING_LITERAL .  107) (OTHERS .  106))
      ((default . error) (END . (case_items . 1)) (WHEN . (case_items . 1)))
      ((default . error) (END .  113) (WHEN .  109))
      ((default . error) (SEMICOLON . (attribute_reference . 1)) (AMPERSAND . (attribute_reference . 1)) (COMMA . (attribute_reference . 1)) (RIGHT_PAREN . (attribute_reference . 1)))
      ((default . error) (CASE .  134))
      ((default . error) (WHEN . (case_items . 2)) (END . (case_items . 2)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 0)) (VERTICAL_BAR . (discrete_choice_list . 0)))
      ((default . error) (VERTICAL_BAR .  133) (EQUAL_GREATER .  132))
      ((default . error) (USE .  131))
      ((default . error) (WHEN . (attribute_declaration . 0)) (END . (attribute_declaration . 0)) (IDENTIFIER . (attribute_declaration . 0)) (TYPE . (attribute_declaration . 0)) (PACKAGE . (attribute_declaration . 0)) (NULL . (attribute_declaration . 0)) (FOR . (attribute_declaration . 0)) (CASE . (attribute_declaration . 0)))
      ((default . error) (USE .  130))
      ((default . error) (END . (declarative_items_opt . 0)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (WHEN . (package_renaming . 0)) (END . (package_renaming . 0)) (IDENTIFIER . (package_renaming . 0)) (TYPE . (package_renaming . 0)) (PACKAGE . (package_renaming . 0)) (NULL . (package_renaming . 0)) (FOR . (package_renaming . 0)) (CASE . (package_renaming . 0)))
      ((default . error) (WHEN . (typed_string_declaration . 0)) (END . (typed_string_declaration . 0)) (CASE . (typed_string_declaration . 0)) (FOR . (typed_string_declaration . 0)) (NULL . (typed_string_declaration . 0)) (PACKAGE . (typed_string_declaration . 0)) (TYPE . (typed_string_declaration . 0)) (IDENTIFIER . (typed_string_declaration . 0)))
      ((default . error) (AMPERSAND .  40) (SEMICOLON .  127))
      ((default . error) (SEMICOLON .  126))
      ((default . error) ($EOI . (project_extension . 0)))
      ((default . error) (WHEN . (simple_declarative_item . 1)) (IDENTIFIER . (simple_declarative_item . 1)) (TYPE . (simple_declarative_item . 1)) (PACKAGE . (simple_declarative_item . 1)) (NULL . (simple_declarative_item . 1)) (FOR . (simple_declarative_item . 1)) (CASE . (simple_declarative_item . 1)) (END . (simple_declarative_item . 1)))
      ((default . error) (SEMICOLON .  141))
      ((default . error) (END .  140))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (LEFT_PAREN .  19) (STRING_LITERAL .  22) (EXTERNAL .  17) (EXTERNAL_AS_LIST .  18) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  21) (PROJECT .  20))
      ((default . error) (END . (declarative_items_opt . 0)) (WHEN . (declarative_items_opt . 0)) (TYPE .  57) (IDENTIFIER .  58) (NULL .  55) (CASE .  53) (FOR .  54) (PACKAGE .  56))
      ((default . error) (EQUAL_GREATER . (discrete_choice . 0)) (VERTICAL_BAR . (discrete_choice . 0)) (STRING_LITERAL .  107) (OTHERS .  106))
      ((default . error) (SEMICOLON .  135))
      ((default . error) (WHEN . (case_statement . 0)) (END . (case_statement . 0)) (IDENTIFIER . (case_statement . 0)) (TYPE . (case_statement . 0)) (PACKAGE . (case_statement . 0)) (NULL . (case_statement . 0)) (FOR . (case_statement . 0)) (CASE . (case_statement . 0)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 1)) (VERTICAL_BAR . (discrete_choice_list . 1)))
      ((default . error) (END . (case_item . 0)) (WHEN . (case_item . 0)))
      ((default . error) (AMPERSAND .  40) (SEMICOLON .  144))
      ((default . error) (AMPERSAND .  40) (SEMICOLON .  143))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  21))
      ((default . error) (WHEN . (package_spec . 0)) (END . (package_spec . 0)) (IDENTIFIER . (package_spec . 0)) (TYPE . (package_spec . 0)) (PACKAGE . (package_spec . 0)) (NULL . (package_spec . 0)) (FOR . (package_spec . 0)) (CASE . (package_spec . 0)))
      ((default . error) (SEMICOLON .  145))
      ((default . error) (WHEN . (attribute_declaration . 2)) (CASE . (attribute_declaration . 2)) (FOR . (attribute_declaration . 2)) (NULL . (attribute_declaration . 2)) (PACKAGE . (attribute_declaration . 2)) (TYPE . (attribute_declaration . 2)) (IDENTIFIER . (attribute_declaration . 2)) (END . (attribute_declaration . 2)))
      ((default . error) (WHEN . (attribute_declaration . 1)) (CASE . (attribute_declaration . 1)) (FOR . (attribute_declaration . 1)) (NULL . (attribute_declaration . 1)) (PACKAGE . (attribute_declaration . 1)) (TYPE . (attribute_declaration . 1)) (IDENTIFIER . (attribute_declaration . 1)) (END . (attribute_declaration . 1)))
      ((default . error) (WHEN . (package_extension . 0)) (END . (package_extension . 0)) (IDENTIFIER . (package_extension . 0)) (TYPE . (package_extension . 0)) (PACKAGE . (package_extension . 0)) (NULL . (package_extension . 0)) (FOR . (package_extension . 0)) (CASE . (package_extension . 0)))]
     [((compilation_unit . 8)(context_clause . 9)(context_clause_opt . 10)(project_extension . 11)(simple_project_declaration . 12)(with_clause . 13))
      nil
      nil
      nil
      nil
      ((identifier_opt . 33))
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 26)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(string_list . 31)(term . 32))
      nil
      ((with_clause . 15))
      ((project_qualifier_opt . 14))
      nil
      nil
      nil
      ((project_declaration_opt . 47)(project_extension . 11)(simple_project_declaration . 12))
      nil
      nil
      ((aggregate . 46))
      ((aggregate . 45))
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 26)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(string_list . 43)(term . 32))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 61)(declarative_items . 62)(declarative_items_opt . 63)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 52)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      nil
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 50))
      nil
      nil
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 26)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(string_list . 43)(term . 32))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((identifier_opt . 28)(name . 81))
      nil
      nil
      ((identifier_opt . 77))
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 73)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 61)(declarative_items . 62)(declarative_items_opt . 95)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      ((identifier_opt . 94))
      nil
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 92)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((case_item . 110)(case_items . 111))
      ((discrete_choice . 108))
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 105)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      nil
      ((identifier_opt . 28)(name . 103))
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 61)(declarative_items . 62)(declarative_items_opt . 102)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      ((identifier_opt . 28)(name . 101))
      ((aggregate . 100))
      nil
      nil
      nil
      nil
      ((identifier_opt . 125))
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 124)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((discrete_choice . 115)(discrete_choice_list . 116))
      nil
      ((case_item . 114))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 61)(declarative_items . 62)(declarative_items_opt . 129)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      ((identifier_opt . 128))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 139)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      ((aggregate . 23)(attribute_prefix . 24)(attribute_reference . 25)(expression . 138)(external_value . 27)(identifier_opt . 28)(name . 29)(string_primary . 30)(term . 32))
      ((attribute_declaration . 59)(case_statement . 60)(declarative_item . 61)(declarative_items . 62)(declarative_items_opt . 137)(package_declaration . 64)(package_spec . 65)(package_extension . 66)(package_renaming . 67)(simple_declarative_item . 68)(typed_string_declaration . 69))
      ((discrete_choice . 136))
      nil
      nil
      nil
      nil
      nil
      nil
      ((identifier_opt . 142))
      nil
      nil
      nil
      nil
      nil]))
  "Parser table.")

(provide 'gpr-grammar-wy)

;; end of file
