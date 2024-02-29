;; This is work derived by Frank Huttner from Bison, the GNU Compiler Compiler.
;; Copyright (C) 1992, 1998-1999, 2003-2005, 2008-2013 Free Software
;; Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:buffalo)

;;; Collection of tests from the GNU Bison v3.0 Testsuite
;;; Run (buffalo:test) to run the collection

;;; Tutorial calculator
(DEFINE-GRAMMAR TS-049
 (:TERMINALS
  (|error| NEG |number| ^ = / - + * |)| |(| ! |\\n|))
 (:PRECEDENCE
  ((:RIGHT ^) (:NONASSOC NEG) (:LEFT / *) (:LEFT - +) (:NONASSOC =)))
 (:START-SYMBOL |input|) (|input| (|line|)) (|input| (|input| |line|))
 (|line| (|\\n|)) (|line| (|exp| |\\n|)) (|exp| (|number|))
 (|exp| (|exp| = |exp|)) (@1 NIL) (|exp| (|exp| + @1 |exp|))
 (|exp| (|exp| - |exp|)) (|exp| (|exp| * |exp|)) (|exp| (|exp| / |exp|))
 (|exp| (:%PREC NEG - |exp|)) (|exp| (|exp| ^ |exp|))
 (|exp| (|(| |exp| |)|)) (|exp| (|(| |error| |)|)) (|exp| (!))
 (|exp| (- |error|)))


;;; Graph with no conflicts
(DEFINE-GRAMMAR TS-101 (:TERMINALS (|b| ?)) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|a| ? b)) (|a| NIL) (b (|b|)))


;;; Graph with unsolved S/R
;; Bison reports empty_* rules on lhs as useless but not the same on rhs
(DEFINE-GRAMMAR TS-102 (:TERMINALS (|c| |b| |a|)) (:PRECEDENCE NIL)
 (:START-SYMBOL |start|) (|start| (|a|)) (|start| (|empty_a| |a|))
 (|start| (|b|)) (|start| (|empty_b| |b|)) (|start| (|c|))
 (|start| (|empty_c| |c|)) (|empty_a| (:%PREC |a|))
 (|empty_b| (:%PREC |b|)) (|empty_c| (:%PREC |c|)))


;;; Graph with solved S/R
;; Bison reports empty_* rules on lhs as useless but not the same on rhs
;; Bison output despite '--define=lr.keep-unreachable-state=true' with 11 states,
;; xml o.k.
(DEFINE-GRAMMAR TS-103 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:RIGHT |c|) (:RIGHT |b|) (:LEFT |a|)))
 (:START-SYMBOL |start|) (|start| (|a|)) (|start| (|empty_a| |a|))
 (|start| (|b|)) (|start| (|empty_b| |b|)) (|start| (|c|))
 (|start| (|empty_c| |c|)) (|empty_a| (:%PREC |a|))
 (|empty_b| (:%PREC |b|)) (|empty_c| (:%PREC |c|)))


;;; Graph with R/R
;; Bison: Nonterminal a is not useful, there is also a non-reachable state
(DEFINE-GRAMMAR TS-104 (:TERMINALS NIL) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|a|)) (|exp| (|b|)) (|a| NIL) (|b| NIL))


;;; Graph with a reduction rule both enabled and disabled graph
(DEFINE-GRAMMAR TS-106 (:TERMINALS (|else| |then| |if| |0| +))
 (:PRECEDENCE NIL) (:START-SYMBOL |exp|) (|exp| (|ifexp|))
 (|exp| (|opexp|)) (|exp| (|imm|))
 (|ifexp| (|if| |exp| |then| |exp| |elseexp|)) (|elseexp| (|else| |exp|))
 (|elseexp| NIL) (|opexp| (|exp| + |exp|)) (|imm| (|0|)))


;;; Nullable
(DEFINE-GRAMMAR TS-113 (:TERMINALS (|e|)) (:PRECEDENCE NIL)
 (:START-SYMBOL e) (e (|e|)) (e NIL))


;;; Broken Closure
(DEFINE-GRAMMAR TS-114 (:TERMINALS (h)) (:PRECEDENCE NIL)
 (:START-SYMBOL |a|) (|a| (|b|)) (|b| (|c|)) (|c| (|d|)) (|d| (|e|))
 (|e| (|f|)) (|f| (|g|)) (|g| (|h|)) (|h| (h)))


;;; Firsts
(DEFINE-GRAMMAR TS-115 (:TERMINALS (exp ^ > = < - +))
 (:PRECEDENCE ((:RIGHT ^ =) (:LEFT - +) (:NONASSOC > <)))
 (:START-SYMBOL |exp|) (|exp| (|exp| < |exp|)) (|exp| (|exp| > |exp|))
 (|exp| (|exp| + |exp|)) (|exp| (|exp| - |exp|)) (|exp| (|exp| ^ |exp|))
 (|exp| (|exp| = |exp|)) (|exp| (exp)))


;;;Useless Terminals
(DEFINE-GRAMMAR TS-117
 (:TERMINALS
  (|useful| |useless9| |useless8| |useless7| |useless6| |useless5|
   |useless4| |useless3| |useless2| |useless1|))
 (:PRECEDENCE NIL) (:START-SYMBOL |exp|) (|exp| (|useful|)))


;;; Useless Rules
(DEFINE-GRAMMAR TS-119
 (:TERMINALS (|useful| |9| |8| |7| |6| |5| |4| |3| |2| |1|))
 (:PRECEDENCE NIL) (:START-SYMBOL |exp|) (|exp| (|useful|))
 (|useless1| (|1|)) (|useless2| (|2|)) (|useless3| (|3|))
 (|useless4| (|4|)) (|useless5| (|5|)) (|useless6| (|6|))
 (|useless7| (|7|)) (|useless8| (|8|)) (|useless9| (|9|)))


;;; Underivable Rules
;; Bison removes underivable rules beforehand, so get only 4 states and
;; no reduce/reduce conflicts
(DEFINE-GRAMMAR TS-121 (:TERMINALS (|useful|)) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|useful|)) (|exp| (|underivable|))
 (|underivable| (|indirection|)) (|indirection| (|underivable|)))


;;; lr.type=lalr: Single State Split
(DEFINE-GRAMMAR TS-124 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a|)) (A (|a|)) (c (|a| |b|))
 (c (A)))


;;; lr.type=canonical-lr: Single State Split
(DEFINE-GRAMMAR TS-126 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a|)) (A (|a|)) (c (|a| |b|))
 (c (A)))


;;; lr.type=lalr: Lane Split
(DEFINE-GRAMMAR TS-128 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a| |a|)) (A (|a| |a|))
 (c (|a| |a| |b|)) (c (A)))


;;; lr.type=canonical-lr: Lane Split
(DEFINE-GRAMMAR TS-130 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a| |a|)) (A (|a| |a|))
 (c (|a| |a| |b|)) (c (A)))


;;; lr.type=lalr: Complex Lane Split
(DEFINE-GRAMMAR TS-132 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a| B)) (B (|a|))
 (B (:%PREC |a|)) (c (|a| |a| |b|)) (c (A)))


;;; lr.type=canonical-lr: Complex Lane Split
(DEFINE-GRAMMAR TS-134 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:LEFT |a|))) (:START-SYMBOL S) (S (|a| A |a|))
 (S (|b| A |b|)) (S (|c| c)) (A (|a| |a| B)) (B (|a|))
 (B (:%PREC |a|)) (c (|a| |a| |b|)) (c (A)))


;;; lr.type=lalr: Split During Added Lookahead Propagation
(DEFINE-GRAMMAR TS-136 (:TERMINALS (|g| |f| |e| |d| |c| |b| |a|))
 (:PRECEDENCE NIL) (:START-SYMBOL S) (S (|a| A |f|)) (S (|a| B))
 (S (|b| A |f|)) (S (|b| B |g|)) (S (|b| |d|)) (S (|c| |c| A |g|))
 (S (|c| |c| B)) (A (|d| |e|)) (B (|d| |e|)))


;;; lr.type=canonical-lr: Split During Added Lookahead Propagation
(DEFINE-GRAMMAR TS-138 (:TERMINALS (|g| |f| |e| |d| |c| |b| |a|))
 (:PRECEDENCE NIL) (:START-SYMBOL S) (S (|a| A |f|)) (S (|a| B))
 (S (|b| A |f|)) (S (|b| B |g|)) (S (|b| |d|)) (S (|c| |c| A |g|))
 (S (|c| |c| B)) (A (|d| |e|)) (B (|d| |e|)))


;;; no lr.default-reduction
(DEFINE-GRAMMAR TS-139 (:TERMINALS (b a)) (:PRECEDENCE NIL)
 (:START-SYMBOL |start|) (|start| (|a| |b|)) (|start| (|a| |b| a))
 (|start| (|a| |c| b)) (|a| (a)) (|b| NIL) (|c| NIL))


;;; Token declaration order: literals vs. identifiers
(DEFINE-GRAMMAR TS-144
 (:TERMINALS (N M L K F E D C |p| |o| |j| |i| |h| |g| |b| |a|))
 (:PRECEDENCE ((:RIGHT N M |p| |o|) (:RIGHT L K |j| |i|)))
 (:START-SYMBOL |exp|) (|exp| (|a|)) (|exp| (|b|)) (|exp| (C)) (|exp| (D))
 (|exp| (E)) (|exp| (F)) (|exp| (|g|)) (|exp| (|h|)) (|exp| (|i|))
 (|exp| (|j|)) (|exp| (K)) (|exp| (L)) (|exp| (M)) (|exp| (N))
 (|exp| (|o|)) (|exp| (|p|)))


;;; Useless precedence warning
(DEFINE-GRAMMAR TS-146 (:TERMINALS (Z Y X W V U B A))
 (:PRECEDENCE
  ((:NONASSOC U) (:RIGHT V) (:LEFT W) (:PRECEDENCE Y) (:LEFT X)
   (:PRECEDENCE Z)))
 (:START-SYMBOL |a|) (|a| (|b|)) (|a| (|a| U |b|)) (|a| (|f|)) (|b| (|c|))
 (|b| (|b| V |c|)) (|c| (|d|)) (|c| (|c| W |d|)) (|d| (A))
 (|d| (|d| X |d|)) (|d| (|d| Y A)) (|f| (B)) (|f| (|f| Z B)))


;;; S/R in initial
(DEFINE-GRAMMAR TS-147 (:TERMINALS (|e|)) (:PRECEDENCE NIL)		
 (:START-SYMBOL |exp|) (|exp| (e |e|)) (e (|e|)) (e NIL))


;;; LAC: %nonassoc requires splitting canonical LR states
(DEFINE-GRAMMAR TS-150 (:TERMINALS (|c| |b| |a|))
 (:PRECEDENCE ((:NONASSOC |a|))) (:START-SYMBOL |start|)
 (|start| (|a| |problem| |a|)) (|start| (|b| |problem| |b|))
 (|start| (|c| |reduce-nonassoc|)) (|problem| (|look| |reduce-nonassoc|))
 (|problem| (|look| |a|)) (|problem| (|look| |b|)) (|look| (|a|))
 (|look| (|a| |b|)) (|look| (|a| |c|)) (|reduce-nonassoc| (:%PREC |a|)))


;;; Unresolved SR Conflicts
(DEFINE-GRAMMAR TS-151 (:TERMINALS (OP NUM)) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|exp| OP |exp|)) (|exp| (NUM)))


;;; Resolved SR Conflicts
(DEFINE-GRAMMAR TS-152 (:TERMINALS (OP NUM)) (:PRECEDENCE ((:LEFT OP)))
 (:START-SYMBOL |exp|) (|exp| (|exp| OP |exp|)) (|exp| (NUM)))


;;; %precedence suffices
(DEFINE-GRAMMAR TS-153 (:TERMINALS (|exp| stmt |if| |else| |then|))
 (:PRECEDENCE ((:PRECEDENCE |else|) (:PRECEDENCE |then|)))
 (:START-SYMBOL |stmt|) (|stmt| (|if| |cond| |then| |stmt|))
 (|stmt| (|if| |cond| |then| |stmt| |else| |stmt|)) (|stmt| (stmt))
 (|cond| (|exp|)))


;;; %precedence does not suffice
(DEFINE-GRAMMAR TS-154 (:TERMINALS (|exp| stmt |if| |else| |then|))
 (:PRECEDENCE ((:PRECEDENCE |else|) (:PRECEDENCE |then|)))
 (:START-SYMBOL |stmt|) (|stmt| (|if| |cond| |then| |stmt|))
 (|stmt| (|if| |cond| |then| |stmt| |else| |stmt|)) (|stmt| (stmt))
 (|cond| (|exp|)) (|cond| (|cond| |then| |cond|)))


;;; Defaulted Conflicted Reduction
;; Bison does not report id and exp -> id as useless
(DEFINE-GRAMMAR TS-155 (:TERMINALS (|0|)) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|num|)) (|exp| (|id|)) (|num| (|0|))
 (|id| (|0|)))


;;; %expect right
(DEFINE-GRAMMAR TS-157 (:TERMINALS (OP NUM)) (:PRECEDENCE NIL)
 (:START-SYMBOL |exp|) (|exp| (|exp| OP |exp|)) (|exp| (NUM)))


;;; %expect with reduce conflicts
(DEFINE-GRAMMAR TS-159 (:TERMINALS (a)) (:PRECEDENCE NIL)
 (:START-SYMBOL |program|) (|program| (|a| a)) (|program| (|a| |a|))
 (|a| (a)))


;;; %no-default-prec with %prec
(DEFINE-GRAMMAR TS-162 (:TERMINALS (|0| + *))
 (:PRECEDENCE ((:LEFT *) (:LEFT +))) (:START-SYMBOL |e|)
 (|e| (:%PREC |+| |e| + |e|)) (|e| (:%PREC |*| |e| * |e|)) (|e| (|0|)))


;;; Solved conflicts report for multiple reductions in a state
;; Bison treats a conflict on an empty rule A -> . as a shift/reduction
;; conflict or a conflict that is resolved based on precedence of the
;; lookahead/rules? rather than which rules comes earlier in the grammar
;; reports on used rules and nonterminals are wrong in Bison
#|
(DEFINE-GRAMMAR TS-165 (:TERMINALS (|d| |c| |b| |a|))
 (:PRECEDENCE ((:RIGHT |d|) (:RIGHT |c|) (:RIGHT |b|) (:LEFT |a|)))
 (:START-SYMBOL |start|) (|start| (|a|)) (|start| (|empty_a| |a|))
 (|start| (|b|)) (|start| (|empty_b| |b|)) (|start| (|c|))
 (|start| (|empty_c1| |c|)) (|start| (|empty_c2| |c|))
 (|start| (|empty_c3| |c|)) (|empty_a| (:%PREC |a|))
 (|empty_b| (:%PREC |b|)) (|empty_c1| (:%PREC |c|))
 (|empty_c2| (:%PREC |c|)) (|empty_c3| (:%PREC |d|)))
|#

;;; Mid-rule actions
(DEFINE-GRAMMAR TS-185 (:TERMINALS (|9| |8| |7| |6| |5| |4| |3| |2| |1|))
 (:PRECEDENCE NIL) (:START-SYMBOL |exp|) ($@1 NIL) ($@2 NIL) ($@3 NIL)
 ($@4 NIL) ($@5 NIL) ($@6 NIL) ($@7 NIL) ($@8 NIL) ($@9 NIL) ($@10 NIL)
 (|exp|
  ($@1 |1| $@2 |2| $@3 |3| $@4 |4| $@5 |5| $@6 |6| $@7 |7| $@8 |8| $@9 |9|
       $@10)))


;;; GNU AWK 3.1.0 Grammar: LALR(1) and Canonical LR(1)
(DEFINE-GRAMMAR TS-322
 (:TERMINALS
  (|error| UNARY TWOWAYIO LEX_LENGTH LEX_BUILTIN DECREMENT INCREMENT
   LEX_OR LEX_AND LEX_IN LEX_NEXTFILE LEX_GETLINE LEX_FUNCTION LEX_EXIT
   LEX_NEXT LEX_PRINTF LEX_PRINT LEX_CONTINUE LEX_BREAK LEX_FOR LEX_DO
   LEX_WHILE LEX_DELETE LEX_RETURN LEX_ELSE LEX_IF LEX_END LEX_BEGIN
   CONCAT_OP NEWLINE MATCHOP ASSIGNOP APPEND_OP RELOP YSTRING YNUMBER
   ERROR REGEXP NAME FUNC_CALL } |\|| { ^ ] [ ? > < |;| |:| / - |,| + *
   |)| |(| % $ !))
 (:PRECEDENCE
  ((:LEFT |)| |(|) (:LEFT $) (:LEFT DECREMENT INCREMENT) (:RIGHT ^)
   (:RIGHT UNARY !) (:LEFT / * %) (:LEFT - +) (:LEFT YSTRING YNUMBER)
   (:LEFT CONCAT_OP) (:NONASSOC TWOWAYIO APPEND_OP RELOP |\|| > <)
   (:NONASSOC MATCHOP) (:NONASSOC |,|)
   (:LEFT LEX_LENGTH LEX_BUILTIN FUNC_CALL) (:NONASSOC LEX_IN)
   (:LEFT LEX_GETLINE) (:LEFT LEX_AND) (:LEFT LEX_OR) (:RIGHT ? |:|)
   (:RIGHT ASSIGNOP)))
 (:START-SYMBOL |start|) (|start| (|opt_nls| |program| |opt_nls|))
 (|program| (|rule|)) (|program| (|program| |rule|)) (|program| (|error|))
 (|program| (|program| |error|)) (|program| NIL) ($@1 NIL)
 (|rule| (LEX_BEGIN $@1 |action|)) ($@2 NIL)
 (|rule| (LEX_END $@2 |action|)) (|rule| (LEX_BEGIN |statement_term|))
 (|rule| (LEX_END |statement_term|)) (|rule| (|pattern| |action|))
 (|rule| (|action|)) (|rule| (|pattern| |statement_term|))
 (|rule| (|function_prologue| |function_body|)) (|func_name| (NAME))
 (|func_name| (FUNC_CALL)) (|func_name| (|lex_builtin|))
 (|lex_builtin| (LEX_BUILTIN)) (|lex_builtin| (LEX_LENGTH)) ($@3 NIL)
 (|function_prologue|
  (LEX_FUNCTION $@3 |func_name| |(| |opt_param_list| |r_paren| |opt_nls|))
 (|function_body| (|l_brace| |statements| |r_brace| |opt_semi| |opt_nls|))
 (|function_body| (|l_brace| |r_brace| |opt_semi| |opt_nls|))
 (|pattern| (|exp|)) (|pattern| (|exp| |,| |exp|)) ($@4 NIL)
 (|regexp| (/ $@4 REGEXP /))
 (|action| (|l_brace| |statements| |r_brace| |opt_semi| |opt_nls|))
 (|action| (|l_brace| |r_brace| |opt_semi| |opt_nls|))
 (|statements| (|statement|)) (|statements| (|statements| |statement|))
 (|statements| (|error|)) (|statements| (|statements| |error|))
 (|statement_term| (|nls|)) (|statement_term| (|semi| |opt_nls|))
 (|statement| (|semi| |opt_nls|)) (|statement| (|l_brace| |r_brace|))
 (|statement| (|l_brace| |statements| |r_brace|))
 (|statement| (|if_statement|))
 (|statement| (LEX_WHILE |(| |exp| |r_paren| |opt_nls| |statement|))
 (|statement|
  (LEX_DO |opt_nls| |statement| LEX_WHILE |(| |exp| |r_paren| |opt_nls|))
 (|statement|
  (LEX_FOR |(| NAME LEX_IN NAME |r_paren| |opt_nls| |statement|))
 (|statement|
  (LEX_FOR |(| |opt_exp| |semi| |opt_nls| |exp| |semi| |opt_nls| |opt_exp|
   |r_paren| |opt_nls| |statement|))
 (|statement|
  (LEX_FOR |(| |opt_exp| |semi| |opt_nls| |semi| |opt_nls| |opt_exp|
   |r_paren| |opt_nls| |statement|))
 (|statement| (LEX_BREAK |statement_term|))
 (|statement| (LEX_CONTINUE |statement_term|))
 (|statement|
  (|print| |(| |expression_list| |r_paren| |output_redir|
   |statement_term|))
 (|statement|
  (|print| |opt_rexpression_list| |output_redir| |statement_term|))
 (|statement| (LEX_NEXT |statement_term|))
 (|statement| (LEX_NEXTFILE |statement_term|))
 (|statement| (LEX_EXIT |opt_exp| |statement_term|)) ($@5 NIL)
 (|statement| (LEX_RETURN $@5 |opt_exp| |statement_term|))
 (|statement| (LEX_DELETE NAME [ |expression_list| ] |statement_term|))
 (|statement| (LEX_DELETE NAME |statement_term|))
 (|statement| (|exp| |statement_term|)) (|print| (LEX_PRINT))
 (|print| (LEX_PRINTF))
 (|if_statement| (LEX_IF |(| |exp| |r_paren| |opt_nls| |statement|))
 (|if_statement|
  (LEX_IF |(| |exp| |r_paren| |opt_nls| |statement| LEX_ELSE |opt_nls|
   |statement|))
 (|nls| (NEWLINE)) (|nls| (|nls| NEWLINE)) (|opt_nls| NIL)
 (|opt_nls| (|nls|)) (|input_redir| NIL) (|input_redir| (< |simp_exp|))
 (|output_redir| NIL) (|output_redir| (> |exp|))
 (|output_redir| (APPEND_OP |exp|)) (|output_redir| (|\|| |exp|))
 (|output_redir| (TWOWAYIO |exp|)) (|opt_param_list| NIL)
 (|opt_param_list| (|param_list|)) (|param_list| (NAME))
 (|param_list| (|param_list| |comma| NAME)) (|param_list| (|error|))
 (|param_list| (|param_list| |error|))
 (|param_list| (|param_list| |comma| |error|)) (|opt_exp| NIL)
 (|opt_exp| (|exp|)) (|opt_rexpression_list| NIL)
 (|opt_rexpression_list| (|rexpression_list|))
 (|rexpression_list| (|rexp|))
 (|rexpression_list| (|rexpression_list| |comma| |rexp|))
 (|rexpression_list| (|error|))
 (|rexpression_list| (|rexpression_list| |error|))
 (|rexpression_list| (|rexpression_list| |error| |rexp|))
 (|rexpression_list| (|rexpression_list| |comma| |error|))
 (|opt_expression_list| NIL) (|opt_expression_list| (|expression_list|))
 (|expression_list| (|exp|))
 (|expression_list| (|expression_list| |comma| |exp|))
 (|expression_list| (|error|))
 (|expression_list| (|expression_list| |error|))
 (|expression_list| (|expression_list| |error| |exp|))
 (|expression_list| (|expression_list| |comma| |error|)) ($@6 NIL)
 (|exp| (|variable| ASSIGNOP $@6 |exp|))
 (|exp| (|(| |expression_list| |r_paren| LEX_IN NAME))
 (|exp| (|exp| |\|| LEX_GETLINE |opt_variable|))
 (|exp| (|exp| TWOWAYIO LEX_GETLINE |opt_variable|))
 (|exp| (LEX_GETLINE |opt_variable| |input_redir|))
 (|exp| (|exp| LEX_AND |exp|)) (|exp| (|exp| LEX_OR |exp|))
 (|exp| (|exp| MATCHOP |exp|)) (|exp| (|regexp|))
 (|exp| (:%PREC UNARY ! |regexp|)) (|exp| (|exp| LEX_IN NAME))
 (|exp| (|exp| RELOP |exp|)) (|exp| (|exp| < |exp|))
 (|exp| (|exp| > |exp|)) (|exp| (|exp| ? |exp| |:| |exp|))
 (|exp| (|simp_exp|)) (|exp| (:%PREC CONCAT_OP |exp| |simp_exp|))
 ($@7 NIL) (|rexp| (|variable| ASSIGNOP $@7 |rexp|))
 (|rexp| (|rexp| LEX_AND |rexp|)) (|rexp| (|rexp| LEX_OR |rexp|))
 (|rexp| (LEX_GETLINE |opt_variable| |input_redir|)) (|rexp| (|regexp|))
 (|rexp| (:%PREC UNARY ! |regexp|)) (|rexp| (|rexp| MATCHOP |rexp|))
 (|rexp| (|rexp| LEX_IN NAME)) (|rexp| (|rexp| RELOP |rexp|))
 (|rexp| (|rexp| ? |rexp| |:| |rexp|)) (|rexp| (|simp_exp|))
 (|rexp| (:%PREC CONCAT_OP |rexp| |simp_exp|))
 (|simp_exp| (|non_post_simp_exp|)) (|simp_exp| (|simp_exp| ^ |simp_exp|))
 (|simp_exp| (|simp_exp| * |simp_exp|))
 (|simp_exp| (|simp_exp| / |simp_exp|))
 (|simp_exp| (|simp_exp| % |simp_exp|))
 (|simp_exp| (|simp_exp| + |simp_exp|))
 (|simp_exp| (|simp_exp| - |simp_exp|))
 (|simp_exp| (|variable| INCREMENT)) (|simp_exp| (|variable| DECREMENT))
 (|non_post_simp_exp| (:%PREC UNARY ! |simp_exp|))
 (|non_post_simp_exp| (|(| |exp| |r_paren|))
 (|non_post_simp_exp| (LEX_BUILTIN |(| |opt_expression_list| |r_paren|))
 (|non_post_simp_exp| (LEX_LENGTH |(| |opt_expression_list| |r_paren|))
 (|non_post_simp_exp| (LEX_LENGTH))
 (|non_post_simp_exp| (FUNC_CALL |(| |opt_expression_list| |r_paren|))
 (|non_post_simp_exp| (|variable|))
 (|non_post_simp_exp| (INCREMENT |variable|))
 (|non_post_simp_exp| (DECREMENT |variable|))
 (|non_post_simp_exp| (YNUMBER)) (|non_post_simp_exp| (YSTRING))
 (|non_post_simp_exp| (:%PREC UNARY - |simp_exp|))
 (|non_post_simp_exp| (:%PREC UNARY + |simp_exp|)) (|opt_variable| NIL)
 (|opt_variable| (|variable|)) (|variable| (NAME))
 (|variable| (NAME [ |expression_list| ]))
 (|variable| ($ |non_post_simp_exp|)) (|l_brace| ({ |opt_nls|))
 (|r_brace| (} |opt_nls|)) (|r_paren| (|)|)) (|opt_semi| NIL)
 (|opt_semi| (|semi|)) (|semi| (|;|)) (|comma| (|,| |opt_nls|)))


;;; GNU Cim Grammar: LALR(1) and Canonical LR(1)
(DEFINE-GRAMMAR TS-325
 (:TERMINALS
  (|error| HDOT HPRIMARYOPERATOR HFACTOROPERATOR UNEAR HTERMOPERATOR
   HOBJRELOPERATOR HREFRELOPERATOR HVALRELOPERATOR HNOT HAND HOR HIMP HEQV
   HANDTHEN HORELSE HASSIGN HTEXTKONST HREALKONST HCHARACTERKONST
   HINTEGERKONST HBOOLEANKONST HIDENTIFIER HDOTDOTDOT HEXP HINTDIV HDIV
   HMUL HSUB HADD HNER HEQR HENDPAR HBEGPAR HSTATEMENTSEPARATOR
   HLABELSEPARATOR HPAREXPSEPARATOR HASSIGNREF HASSIGNVALUE HWHILE HWHEN
   HVIRTUAL HVAR HVALUE HUNTIL HTO HTHIS HTHEN HTEXT HSWITCH HSTEP HSHORT
   HREF HREAL HREACTIVATE HQUA HPROTECTED HPROCEDURE HPRIOR HOTHERWISE
   HNOTEXT HNONE HNEW HNE HNAME HLT HLONG HLE HLABEL HIS HINTEGER HINSPECT
   HINNER HIN HIF HHIDDEN HGT HGOTO HGO HGE HFOR HEXTERNAL HEQ HEND HELSE
   HDO HDELAY HCONC HCLASS HCHARACTER HBOOLEAN HBEGIN HBEFORE HAT HARRAY
   HAFTER HACTIVATE))
 (:PRECEDENCE
  ((:LEFT HDOT) (:LEFT HQUA) (:LEFT HPRIMARYOPERATOR)
   (:LEFT HFACTOROPERATOR) (:LEFT UNEAR) (:LEFT HTERMOPERATOR)
   (:LEFT HCONC) (:LEFT HOBJRELOPERATOR HREFRELOPERATOR HVALRELOPERATOR)
   (:LEFT HNOT) (:LEFT HAND) (:LEFT HOR) (:LEFT HIMP) (:LEFT HEQV)
   (:LEFT HANDTHEN) (:LEFT HORELSE) (:RIGHT HASSIGN)))
 (:START-SYMBOL MAIN_MODULE) ($@1 NIL) (MAIN_MODULE ($@1 MODULS))
 (MAIN_MODULE (|error| HSTATEMENTSEPARATOR MBEE_DECLSTMS)) ($@2 NIL)
 (EXT_DECLARATION (HEXTERNAL MBEE_TYPE HPROCEDURE $@2 EXT_LIST)) ($@3 NIL)
 ($@4 NIL)
 (EXT_DECLARATION
  (HEXTERNAL HIDENTIFIER HPROCEDURE $@3 HIDENTIFIER $@4
   EXTERNAL_KIND_ITEM))
 ($@5 NIL) (EXT_DECLARATION (HEXTERNAL HCLASS $@5 EXT_LIST)) ($@6 NIL)
 ($@7 NIL)
 (EXTERNAL_KIND_ITEM
  (EXT_IDENT HOBJRELOPERATOR $@6 MBEE_TYPE HPROCEDURE HIDENTIFIER $@7
   HEADING EMPTY_BLOCK))
 (EMPTY_BLOCK NIL) (EMPTY_BLOCK (HBEGIN HEND)) (EXT_LIST (EXT_ITEM))
 (EXT_LIST (EXT_LIST HPAREXPSEPARATOR EXT_ITEM))
 (EXT_ITEM (HIDENTIFIER EXT_IDENT)) (EXT_IDENT NIL) ($@8 NIL)
 (EXT_IDENT (HVALRELOPERATOR $@8 HTEXTKONST)) (NO_TYPE NIL)
 (MBEE_TYPE (NO_TYPE)) (MBEE_TYPE (TYPE)) ($@9 NIL)
 (TYPE (HREF HBEGPAR HIDENTIFIER $@9 HENDPAR)) (TYPE (HTEXT))
 (TYPE (HBOOLEAN)) (TYPE (HCHARACTER)) (TYPE (HSHORT HINTEGER))
 (TYPE (HINTEGER)) (TYPE (HREAL)) (TYPE (HLONG HREAL))
 (MBEE_ELSE_PART NIL) ($@10 NIL) (MBEE_ELSE_PART (HELSE $@10 BLOCK))
 (FOR_LIST (FOR_LIST_ELEMENT))
 (FOR_LIST (FOR_LIST_ELEMENT HPAREXPSEPARATOR FOR_LIST))
 (FOR_LIST_ELEMENT (EXPRESSION MBEE_F_L_EL_R_PT)) (MBEE_F_L_EL_R_PT NIL)
 (MBEE_F_L_EL_R_PT (HWHILE EXPRESSION))
 (MBEE_F_L_EL_R_PT (HSTEP EXPRESSION HUNTIL EXPRESSION)) (GOTO (HGO HTO))
 (GOTO (HGOTO)) (CONN_STATE_R_PT (WHEN_CLAUSE_LIST)) ($@11 NIL)
 (CONN_STATE_R_PT (HDO $@11 BLOCK)) ($@12 NIL)
 (WHEN_CLAUSE_LIST (HWHEN HIDENTIFIER HDO $@12 BLOCK)) ($@13 NIL)
 (WHEN_CLAUSE_LIST (WHEN_CLAUSE_LIST HWHEN HIDENTIFIER HDO $@13 BLOCK))
 (MBEE_OTWI_CLAUS NIL) ($@14 NIL)
 (MBEE_OTWI_CLAUS (HOTHERWISE $@14 BLOCK)) (ACTIVATOR (HACTIVATE))
 (ACTIVATOR (HREACTIVATE)) (SCHEDULE NIL) ($@15 NIL)
 (SCHEDULE (ATDELAY EXPRESSION $@15 PRIOR)) ($@16 NIL)
 (SCHEDULE (BEFOREAFTER $@16 EXPRESSION)) (ATDELAY (HAT))
 (ATDELAY (HDELAY)) (BEFOREAFTER (HBEFORE)) (BEFOREAFTER (HAFTER))
 (PRIOR NIL) (PRIOR (HPRIOR)) ($@17 NIL)
 (MODULSTATEMENT (HWHILE EXPRESSION HDO $@17 BLOCK)) ($@18 NIL) ($@19 NIL)
 (MODULSTATEMENT (HIF EXPRESSION HTHEN $@18 BLOCK $@19 MBEE_ELSE_PART))
 ($@20 NIL) ($@21 NIL)
 (MODULSTATEMENT (HFOR HIDENTIFIER HASSIGN $@20 FOR_LIST HDO $@21 BLOCK))
 (MODULSTATEMENT (GOTO EXPRESSION)) ($@22 NIL) ($@23 NIL)
 (MODULSTATEMENT
  (HINSPECT EXPRESSION $@22 CONN_STATE_R_PT $@23 MBEE_OTWI_CLAUS))
 (MODULSTATEMENT (HINNER)) ($@24 NIL)
 (MODULSTATEMENT (HIDENTIFIER HLABELSEPARATOR $@24 DECLSTATEMENT))
 ($@25 NIL) ($@26 NIL)
 (MODULSTATEMENT
  (EXPRESSION_SIMP HBEGIN $@25 IMPORT_SPEC_MODULE $@26 MBEE_DECLSTMS
   HEND))
 (MODULSTATEMENT
  (EXPRESSION_SIMP HBEGIN |error| HSTATEMENTSEPARATOR MBEE_DECLSTMS HEND))
 (MODULSTATEMENT (EXPRESSION_SIMP HBEGIN |error| HEND))
 (MODULSTATEMENT (EXPRESSION_SIMP))
 (MODULSTATEMENT (ACTIVATOR EXPRESSION SCHEDULE)) ($@27 NIL)
 (MODULSTATEMENT (HBEGIN $@27 MBEE_DECLSTMS HEND)) ($@28 NIL)
 (MODULSTATEMENT (MBEE_TYPE HPROCEDURE HIDENTIFIER $@28 HEADING BLOCK))
 ($@29 NIL) ($@30 NIL)
 (MODULSTATEMENT
  (HIDENTIFIER HCLASS NO_TYPE $@29 IMPORT_SPEC_MODULE HIDENTIFIER $@30
   HEADING BLOCK))
 ($@31 NIL)
 (MODULSTATEMENT (HCLASS NO_TYPE HIDENTIFIER $@31 HEADING BLOCK))
 (MODULSTATEMENT (EXT_DECLARATION)) (MODULSTATEMENT NIL)
 (IMPORT_SPEC_MODULE NIL) (DECLSTATEMENT (MODULSTATEMENT)) ($@32 NIL)
 (DECLSTATEMENT
  (TYPE HIDENTIFIER MBEE_CONSTANT HPAREXPSEPARATOR $@32 IDENTIFIER_LISTC))
 (DECLSTATEMENT (TYPE HIDENTIFIER MBEE_CONSTANT)) ($@33 NIL)
 (DECLSTATEMENT (MBEE_TYPE HARRAY $@33 ARR_SEGMENT_LIST)) ($@34 NIL)
 (DECLSTATEMENT (HSWITCH HIDENTIFIER HASSIGN $@34 SWITCH_LIST))
 (BLOCK (DECLSTATEMENT)) (BLOCK (HBEGIN MBEE_DECLSTMS HEND))
 (BLOCK (HBEGIN |error| HSTATEMENTSEPARATOR MBEE_DECLSTMS HEND))
 (BLOCK (HBEGIN |error| HEND)) (MBEE_DECLSTMS (MBEE_DECLSTMSU))
 (MBEE_DECLSTMSU (DECLSTATEMENT))
 (MBEE_DECLSTMSU (MBEE_DECLSTMSU HSTATEMENTSEPARATOR DECLSTATEMENT))
 (MODULS (MODULSTATEMENT))
 (MODULS (MODULS HSTATEMENTSEPARATOR MODULSTATEMENT))
 (ARR_SEGMENT_LIST (ARR_SEGMENT))
 (ARR_SEGMENT_LIST (ARR_SEGMENT_LIST HPAREXPSEPARATOR ARR_SEGMENT))
 (ARR_SEGMENT (ARRAY_SEGMENT HBEGPAR BAUND_PAIR_LIST HENDPAR))
 (ARRAY_SEGMENT (ARRAY_SEGMENT_EL))
 (ARRAY_SEGMENT (ARRAY_SEGMENT_EL HPAREXPSEPARATOR ARRAY_SEGMENT))
 (ARRAY_SEGMENT_EL (HIDENTIFIER)) (BAUND_PAIR_LIST (BAUND_PAIR))
 (BAUND_PAIR_LIST (BAUND_PAIR HPAREXPSEPARATOR BAUND_PAIR_LIST))
 (BAUND_PAIR (EXPRESSION HLABELSEPARATOR EXPRESSION))
 (SWITCH_LIST (EXPRESSION))
 (SWITCH_LIST (EXPRESSION HPAREXPSEPARATOR SWITCH_LIST)) ($@35 NIL)
 ($@36 NIL) ($@37 NIL) ($@38 NIL)
 (HEADING
  (MBEE_FMAL_PAR_P HSTATEMENTSEPARATOR $@35 MBEE_MODE_PART $@36
   MBEE_SPEC_PART $@37 MBEE_PROT_PART $@38 MBEE_VIRT_PART))
 (MBEE_FMAL_PAR_P NIL) (MBEE_FMAL_PAR_P (FMAL_PAR_PART))
 (FMAL_PAR_PART (HBEGPAR NO_TYPE MBEE_LISTV HENDPAR)) (MBEE_LISTV NIL)
 (MBEE_LISTV (LISTV)) (LISTV (HIDENTIFIER)) (LISTV (FPP_CATEG HDOTDOTDOT))
 ($@39 NIL) (LISTV (HIDENTIFIER $@39 HPAREXPSEPARATOR LISTV))
 (LISTV (FPP_SPEC)) (LISTV (FPP_SPEC HPAREXPSEPARATOR LISTV))
 (FPP_HEADING (HBEGPAR NO_TYPE FPP_MBEE_LISTV HENDPAR))
 (FPP_MBEE_LISTV NIL) (FPP_MBEE_LISTV (FPP_LISTV))
 (FPP_LISTV (FPP_CATEG HDOTDOTDOT)) (FPP_LISTV (FPP_SPEC))
 (FPP_LISTV (FPP_SPEC HPAREXPSEPARATOR LISTV))
 (FPP_SPEC (FPP_CATEG SPECIFIER HIDENTIFIER))
 (FPP_SPEC (FPP_CATEG FPP_PROC_DECL_IN_SPEC))
 (FPP_CATEG (HNAME HLABELSEPARATOR)) (FPP_CATEG (HVALUE HLABELSEPARATOR))
 (FPP_CATEG (HVAR HLABELSEPARATOR)) (FPP_CATEG NIL) ($@40 NIL) ($@41 NIL)
 (FPP_PROC_DECL_IN_SPEC
  (MBEE_TYPE HPROCEDURE HIDENTIFIER $@40 FPP_HEADING $@41))
 (IDENTIFIER_LISTV (HIDENTIFIER)) (IDENTIFIER_LISTV (HDOTDOTDOT))
 ($@42 NIL)
 (IDENTIFIER_LISTV (HIDENTIFIER $@42 HPAREXPSEPARATOR IDENTIFIER_LISTV))
 (MBEE_MODE_PART NIL) (MBEE_MODE_PART (MODE_PART)) (MODE_PART (NAME_PART))
 (MODE_PART (VALUE_PART)) (MODE_PART (VAR_PART))
 (MODE_PART (NAME_PART VALUE_PART)) (MODE_PART (VALUE_PART NAME_PART))
 (MODE_PART (NAME_PART VAR_PART)) (MODE_PART (VAR_PART NAME_PART))
 (MODE_PART (VALUE_PART VAR_PART)) (MODE_PART (VAR_PART VALUE_PART))
 (MODE_PART (VAR_PART NAME_PART VALUE_PART))
 (MODE_PART (NAME_PART VAR_PART VALUE_PART))
 (MODE_PART (NAME_PART VALUE_PART VAR_PART))
 (MODE_PART (VAR_PART VALUE_PART NAME_PART))
 (MODE_PART (VALUE_PART VAR_PART NAME_PART))
 (MODE_PART (VALUE_PART NAME_PART VAR_PART)) ($@43 NIL)
 (NAME_PART (HNAME $@43 IDENTIFIER_LISTV HSTATEMENTSEPARATOR)) ($@44 NIL)
 (VAR_PART (HVAR $@44 IDENTIFIER_LISTV HSTATEMENTSEPARATOR)) ($@45 NIL)
 (VALUE_PART (HVALUE $@45 IDENTIFIER_LISTV HSTATEMENTSEPARATOR))
 (MBEE_SPEC_PART NIL) (MBEE_SPEC_PART (SPEC_PART)) (SPEC_PART (ONE_SPEC))
 (SPEC_PART (SPEC_PART ONE_SPEC))
 (ONE_SPEC (SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR)) ($@46 NIL)
 (ONE_SPEC
  (NO_TYPE HPROCEDURE HIDENTIFIER HOBJRELOPERATOR $@46 PROC_DECL_IN_SPEC
   HSTATEMENTSEPARATOR))
 (ONE_SPEC (FPP_PROC_DECL_IN_SPEC HSTATEMENTSEPARATOR))
 (ONE_SPEC (MBEE_TYPE HPROCEDURE HIDENTIFIER HSTATEMENTSEPARATOR))
 (ONE_SPEC
  (MBEE_TYPE HPROCEDURE HIDENTIFIER HPAREXPSEPARATOR IDENTIFIER_LIST
   HSTATEMENTSEPARATOR))
 (SPECIFIER (TYPE)) (SPECIFIER (MBEE_TYPE HARRAY)) (SPECIFIER (HLABEL))
 (SPECIFIER (HSWITCH)) ($@47 NIL) ($@48 NIL)
 (PROC_DECL_IN_SPEC
  (MBEE_TYPE HPROCEDURE HIDENTIFIER $@47 HEADING $@48 MBEE_BEGIN_END))
 (MBEE_BEGIN_END NIL) (MBEE_BEGIN_END (HBEGIN HEND)) (MBEE_PROT_PART NIL)
 (MBEE_PROT_PART (PROTECTION_PART))
 (PROTECTION_PART (PROT_SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR))
 (PROTECTION_PART
  (PROTECTION_PART PROT_SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR))
 (PROT_SPECIFIER (HHIDDEN)) (PROT_SPECIFIER (HPROTECTED))
 (PROT_SPECIFIER (HHIDDEN HPROTECTED))
 (PROT_SPECIFIER (HPROTECTED HHIDDEN)) (MBEE_VIRT_PART NIL)
 (MBEE_VIRT_PART (VIRTUAL_PART))
 (VIRTUAL_PART (HVIRTUAL HLABELSEPARATOR MBEE_SPEC_PART))
 (IDENTIFIER_LIST (HIDENTIFIER))
 (IDENTIFIER_LIST (IDENTIFIER_LIST HPAREXPSEPARATOR HIDENTIFIER))
 (IDENTIFIER_LISTC (HIDENTIFIER MBEE_CONSTANT))
 (IDENTIFIER_LISTC
  (IDENTIFIER_LISTC HPAREXPSEPARATOR HIDENTIFIER MBEE_CONSTANT))
 (MBEE_CONSTANT NIL) ($@49 NIL)
 (MBEE_CONSTANT (HVALRELOPERATOR $@49 EXPRESSION))
 (EXPRESSION (EXPRESSION_SIMP))
 (EXPRESSION (HIF EXPRESSION HTHEN EXPRESSION HELSE EXPRESSION))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HASSIGN EXPRESSION))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HCONC EXPRESSION_SIMP))
 (EXPRESSION_SIMP
  (:%PREC HORELSE EXPRESSION_SIMP HOR HELSE EXPRESSION_SIMP))
 (EXPRESSION_SIMP
  (:%PREC HANDTHEN EXPRESSION_SIMP HAND HTHEN EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HEQV EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HIMP EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HAND EXPRESSION_SIMP))
 (EXPRESSION_SIMP (HNOT EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HVALRELOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HREFRELOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HOBJRELOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (:%PREC UNEAR HTERMOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HTERMOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HFACTOROPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HPRIMARYOPERATOR EXPRESSION_SIMP))
 (EXPRESSION_SIMP (HBEGPAR EXPRESSION HENDPAR))
 (EXPRESSION_SIMP (HTEXTKONST)) (EXPRESSION_SIMP (HCHARACTERKONST))
 (EXPRESSION_SIMP (HREALKONST)) (EXPRESSION_SIMP (HINTEGERKONST))
 (EXPRESSION_SIMP (HBOOLEANKONST)) (EXPRESSION_SIMP (HNONE)) ($@50 NIL)
 (EXPRESSION_SIMP (HIDENTIFIER $@50 MBEE_ARG_R_PT))
 (EXPRESSION_SIMP (HTHIS HIDENTIFIER))
 (EXPRESSION_SIMP (HNEW HIDENTIFIER ARG_R_PT))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HDOT EXPRESSION_SIMP))
 (EXPRESSION_SIMP (EXPRESSION_SIMP HQUA HIDENTIFIER)) (ARG_R_PT NIL)
 (ARG_R_PT (HBEGPAR ARGUMENT_LIST HENDPAR)) (MBEE_ARG_R_PT NIL)
 (MBEE_ARG_R_PT (HBEGPAR ARGUMENT_LIST HENDPAR))
 (ARGUMENT_LIST (EXPRESSION))
 (ARGUMENT_LIST (EXPRESSION HPAREXPSEPARATOR ARGUMENT_LIST)))


;;; GNU pic (Groff 1.18.1) Grammar: LALR(1) and Canonical LR(1)
(DEFINE-GRAMMAR TS-328
 (:TERMINALS
  (COMMAND SPRINTF ALIGNED SHADED OUTLINED COLORED FILL THICKNESS PLOT
   UNTIL RESET START END CENTER WEST EAST SOUTH NORTH RIGHT_CORNER
   LEFT_CORNER GREATEREQUAL LESSEQUAL EQUALEQUAL NOTEQUAL OROR ANDAND ELSE
   IF DO FOR CCW CW PRINT SH LOWER UPPER BOTTOM TOP THROUGH COPY SRAND
   RAND INT K_MIN K_MAX SQRT EXP LOG ATAN2 COS SIN DOT_RAD DOT_WID DOT_HT
   DOT_Y DOT_X DOT_END DOT_START DOT_C DOT_SW DOT_NW DOT_SE DOT_NE DOT_S
   DOT_W DOT_E DOT_N HERE AND BETWEEN WAY THE OF BELOW ABOVE RJUST LJUST
   INVISIBLE SAME CHOP DASHED DOTTED SOLID THEN BY WITH AT TO FROM
   DIAMETER WIDTH RADIUS HEIGHT SPLINE MOVE ARROW LINE ARC ELLIPSE CIRCLE
   BOX RIGHT LEFT DOWN UP LAST DOUBLE_ARROW_HEAD RIGHT_ARROW_HEAD
   LEFT_ARROW_HEAD TH ORDINAL DELIMITED COMMAND_LINE TEXT NUMBER VARIABLE
   LABEL } { |`| ^ ] [ > = < |;| |:| / |.| - |,| + * |)| |(| % !))
 (:PRECEDENCE
  ((:RIGHT ^) (:RIGHT !) (:LEFT / * %) (:LEFT - +) (:LEFT AND)
   (:LEFT BETWEEN OF) (:LEFT GREATEREQUAL LESSEQUAL > <)
   (:LEFT EQUALEQUAL NOTEQUAL) (:LEFT ANDAND) (:LEFT OROR) (:LEFT |,|)
   (:LEFT START END CENTER WEST EAST SOUTH NORTH LOWER UPPER)
   (:LEFT RIGHT_CORNER LEFT_CORNER BOTTOM TOP DOT_END DOT_START)
   (:LEFT DOT_C DOT_SW DOT_NW DOT_SE DOT_NE DOT_S DOT_W DOT_E DOT_N)
   (:LEFT THICKNESS AT TO FROM DIAMETER WIDTH RADIUS HEIGHT)
   (:LEFT SPLINE ARROW LINE ARC ELLIPSE CIRCLE BOX [)
   (:LEFT HERE ORDINAL |`|)
   (:LEFT SRAND RAND INT K_MIN K_MAX SQRT EXP LOG ATAN2 COS SIN LAST
    NUMBER VARIABLE |(|)
   (:LEFT LABEL)
   (:LEFT OUTLINED COLORED FILL CHOP DASHED DOTTED SOLID DOWN UP)
   (:LEFT RIGHT LEFT) (:LEFT BELOW ABOVE RJUST LJUST) (:LEFT SPRINTF TEXT)
   (:LEFT PLOT) (:LEFT |.|)))
 (:START-SYMBOL |top|) (|top| (|optional_separator|))
 (|top| (|element_list|))
 (|element_list|
  (|optional_separator| |middle_element_list| |optional_separator|))
 (|middle_element_list| (|element|))
 (|middle_element_list| (|middle_element_list| |separator| |element|))
 (|optional_separator| NIL) (|optional_separator| (|separator|))
 (|separator| (|;|)) (|separator| (|separator| |;|))
 (|placeless_element| (VARIABLE = |any_expr|))
 (|placeless_element| (VARIABLE |:| = |any_expr|))
 (|placeless_element| (UP)) (|placeless_element| (DOWN))
 (|placeless_element| (LEFT)) (|placeless_element| (RIGHT))
 (|placeless_element| (COMMAND_LINE))
 (|placeless_element| (COMMAND |print_args|))
 (|placeless_element| (PRINT |print_args|)) ($@1 NIL)
 (|placeless_element| (SH $@1 DELIMITED))
 (|placeless_element| (COPY TEXT)) ($@2 NIL) ($@3 NIL)
 (|placeless_element| (COPY TEXT THROUGH $@2 DELIMITED $@3 |until|))
 ($@4 NIL) ($@5 NIL)
 (|placeless_element| (COPY THROUGH $@4 DELIMITED $@5 |until|)) ($@6 NIL)
 (|placeless_element|
  (FOR VARIABLE = |expr| TO |expr| |optional_by| DO $@6 DELIMITED))
 (|placeless_element| (|simple_if|)) ($@7 NIL)
 (|placeless_element| (|simple_if| ELSE $@7 DELIMITED))
 (|placeless_element| (|reset_variables|)) (|placeless_element| (RESET))
 (|reset_variables| (RESET VARIABLE))
 (|reset_variables| (|reset_variables| VARIABLE))
 (|reset_variables| (|reset_variables| |,| VARIABLE))
 (|print_args| (|print_arg|)) (|print_args| (|print_args| |print_arg|))
 (|print_arg| (:%PREC |,| |expr|)) (|print_arg| (|text|))
 (|print_arg| (:%PREC |,| |position|)) ($@8 NIL)
 (|simple_if|
  (IF |any_expr|
      THEN
      $@8
      DELIMITED))
 (|until| NIL) (|until| (UNTIL TEXT)) (|any_expr| (|expr|))
 (|any_expr| (|text_expr|)) (|text_expr| (|text| EQUALEQUAL |text|))
 (|text_expr| (|text| NOTEQUAL |text|))
 (|text_expr| (|text_expr| ANDAND |text_expr|))
 (|text_expr| (|text_expr| ANDAND |expr|))
 (|text_expr| (|expr| ANDAND |text_expr|))
 (|text_expr| (|text_expr| OROR |text_expr|))
 (|text_expr| (|text_expr| OROR |expr|))
 (|text_expr| (|expr| OROR |text_expr|)) (|text_expr| (! |text_expr|))
 (|optional_by| NIL) (|optional_by| (BY |expr|))
 (|optional_by| (BY * |expr|)) (|element| (|object_spec|))
 (|element| (LABEL |:| |optional_separator| |element|))
 (|element| (LABEL |:| |optional_separator| |position_not_place|))
 (|element| (LABEL |:| |optional_separator| |place|)) ($@9 NIL) ($@10 NIL)
 (|element| ({ $@9 |element_list| } $@10 |optional_element|))
 (|element| (|placeless_element|)) (|optional_element| NIL)
 (|optional_element| (|element|)) (|object_spec| (BOX))
 (|object_spec| (CIRCLE)) (|object_spec| (ELLIPSE)) (|object_spec| (ARC))
 (|object_spec| (LINE)) (|object_spec| (ARROW)) (|object_spec| (MOVE))
 (|object_spec| (SPLINE)) (|object_spec| (:%PREC TEXT |text|))
 (|object_spec| (PLOT |expr|)) (|object_spec| (PLOT |expr| |text|))
 ($@11 NIL) (|object_spec| ([ $@11 |element_list| ]))
 (|object_spec| (|object_spec| HEIGHT |expr|))
 (|object_spec| (|object_spec| RADIUS |expr|))
 (|object_spec| (|object_spec| WIDTH |expr|))
 (|object_spec| (|object_spec| DIAMETER |expr|))
 (|object_spec| (:%PREC HEIGHT |object_spec| |expr|))
 (|object_spec| (|object_spec| UP))
 (|object_spec| (|object_spec| UP |expr|))
 (|object_spec| (|object_spec| DOWN))
 (|object_spec| (|object_spec| DOWN |expr|))
 (|object_spec| (|object_spec| RIGHT))
 (|object_spec| (|object_spec| RIGHT |expr|))
 (|object_spec| (|object_spec| LEFT))
 (|object_spec| (|object_spec| LEFT |expr|))
 (|object_spec| (|object_spec| FROM |position|))
 (|object_spec| (|object_spec| TO |position|))
 (|object_spec| (|object_spec| AT |position|))
 (|object_spec| (|object_spec| WITH |path|))
 (|object_spec| (:%PREC |,| |object_spec| WITH |position|))
 (|object_spec| (|object_spec| BY |expr_pair|))
 (|object_spec| (|object_spec| THEN))
 (|object_spec| (|object_spec| SOLID))
 (|object_spec| (|object_spec| DOTTED))
 (|object_spec| (|object_spec| DOTTED |expr|))
 (|object_spec| (|object_spec| DASHED))
 (|object_spec| (|object_spec| DASHED |expr|))
 (|object_spec| (|object_spec| FILL))
 (|object_spec| (|object_spec| FILL |expr|))
 (|object_spec| (|object_spec| SHADED |text|))
 (|object_spec| (|object_spec| COLORED |text|))
 (|object_spec| (|object_spec| OUTLINED |text|))
 (|object_spec| (|object_spec| CHOP))
 (|object_spec| (|object_spec| CHOP |expr|))
 (|object_spec| (|object_spec| SAME))
 (|object_spec| (|object_spec| INVISIBLE))
 (|object_spec| (|object_spec| LEFT_ARROW_HEAD))
 (|object_spec| (|object_spec| RIGHT_ARROW_HEAD))
 (|object_spec| (|object_spec| DOUBLE_ARROW_HEAD))
 (|object_spec| (|object_spec| CW)) (|object_spec| (|object_spec| CCW))
 (|object_spec| (:%PREC TEXT |object_spec| |text|))
 (|object_spec| (|object_spec| LJUST))
 (|object_spec| (|object_spec| RJUST))
 (|object_spec| (|object_spec| ABOVE))
 (|object_spec| (|object_spec| BELOW))
 (|object_spec| (|object_spec| THICKNESS |expr|))
 (|object_spec| (|object_spec| ALIGNED)) (|text| (TEXT))
 (|text| (SPRINTF |(| TEXT |sprintf_args| |)|)) (|sprintf_args| NIL)
 (|sprintf_args| (|sprintf_args| |,| |expr|))
 (|position| (|position_not_place|)) (|position| (|place|))
 (|position_not_place| (|expr_pair|))
 (|position_not_place| (|position| + |expr_pair|))
 (|position_not_place| (|position| - |expr_pair|))
 (|position_not_place| (|(| |position| |,| |position| |)|))
 (|position_not_place| (|expr| |between| |position| AND |position|))
 (|position_not_place| (|expr| < |position| |,| |position| >))
 (|between| (BETWEEN)) (|between| (OF THE WAY BETWEEN))
 (|expr_pair| (|expr| |,| |expr|)) (|expr_pair| (|(| |expr_pair| |)|))
 (|place| (:%PREC CHOP |label|)) (|place| (|label| |corner|))
 (|place| (|corner| |label|)) (|place| (|corner| OF |label|))
 (|place| (HERE)) (|label| (LABEL)) (|label| (|nth_primitive|))
 (|label| (|label| |.| LABEL)) (|ordinal| (ORDINAL))
 (|ordinal| (|`| |any_expr| TH)) (|optional_ordinal_last| (LAST))
 (|optional_ordinal_last| (|ordinal| LAST))
 (|nth_primitive| (|ordinal| |object_type|))
 (|nth_primitive| (|optional_ordinal_last| |object_type|))
 (|object_type| (BOX)) (|object_type| (CIRCLE)) (|object_type| (ELLIPSE))
 (|object_type| (ARC)) (|object_type| (LINE)) (|object_type| (ARROW))
 (|object_type| (SPLINE)) (|object_type| ([ ])) (|object_type| (TEXT))
 (|label_path| (|.| LABEL)) (|label_path| (|label_path| |.| LABEL))
 (|relative_path| (:%PREC CHOP |corner|))
 (|relative_path| (:%PREC TEXT |label_path|))
 (|relative_path| (|label_path| |corner|)) (|path| (|relative_path|))
 (|path| (|(| |relative_path| |,| |relative_path| |)|))
 (|path| (ORDINAL LAST |object_type| |relative_path|))
 (|path| (LAST |object_type| |relative_path|))
 (|path| (ORDINAL |object_type| |relative_path|))
 (|path| (LABEL |relative_path|)) (|corner| (DOT_N)) (|corner| (DOT_E))
 (|corner| (DOT_W)) (|corner| (DOT_S)) (|corner| (DOT_NE))
 (|corner| (DOT_SE)) (|corner| (DOT_NW)) (|corner| (DOT_SW))
 (|corner| (DOT_C)) (|corner| (DOT_START)) (|corner| (DOT_END))
 (|corner| (TOP)) (|corner| (BOTTOM)) (|corner| (LEFT)) (|corner| (RIGHT))
 (|corner| (UPPER LEFT)) (|corner| (LOWER LEFT)) (|corner| (UPPER RIGHT))
 (|corner| (LOWER RIGHT)) (|corner| (LEFT_CORNER))
 (|corner| (RIGHT_CORNER)) (|corner| (UPPER LEFT_CORNER))
 (|corner| (LOWER LEFT_CORNER)) (|corner| (UPPER RIGHT_CORNER))
 (|corner| (LOWER RIGHT_CORNER)) (|corner| (NORTH)) (|corner| (SOUTH))
 (|corner| (EAST)) (|corner| (WEST)) (|corner| (CENTER))
 (|corner| (START)) (|corner| (END)) (|expr| (VARIABLE)) (|expr| (NUMBER))
 (|expr| (|place| DOT_X)) (|expr| (|place| DOT_Y))
 (|expr| (|place| DOT_HT)) (|expr| (|place| DOT_WID))
 (|expr| (|place| DOT_RAD)) (|expr| (|expr| + |expr|))
 (|expr| (|expr| - |expr|)) (|expr| (|expr| * |expr|))
 (|expr| (|expr| / |expr|)) (|expr| (|expr| % |expr|))
 (|expr| (|expr| ^ |expr|)) (|expr| (:%PREC |!| - |expr|))
 (|expr| (|(| |any_expr| |)|)) (|expr| (SIN |(| |any_expr| |)|))
 (|expr| (COS |(| |any_expr| |)|))
 (|expr| (ATAN2 |(| |any_expr| |,| |any_expr| |)|))
 (|expr| (LOG |(| |any_expr| |)|)) (|expr| (EXP |(| |any_expr| |)|))
 (|expr| (SQRT |(| |any_expr| |)|))
 (|expr| (K_MAX |(| |any_expr| |,| |any_expr| |)|))
 (|expr| (K_MIN |(| |any_expr| |,| |any_expr| |)|))
 (|expr| (INT |(| |any_expr| |)|)) (|expr| (RAND |(| |any_expr| |)|))
 (|expr| (RAND |(| |)|)) (|expr| (SRAND |(| |any_expr| |)|))
 (|expr| (|expr| < |expr|)) (|expr| (|expr| LESSEQUAL |expr|))
 (|expr| (|expr| > |expr|)) (|expr| (|expr| GREATEREQUAL |expr|))
 (|expr| (|expr| EQUALEQUAL |expr|)) (|expr| (|expr| NOTEQUAL |expr|))
 (|expr| (|expr| ANDAND |expr|)) (|expr| (|expr| OROR |expr|))
 (|expr| (! |expr|)))


;;; Trivial grammars
(DEFINE-GRAMMAR TS-329 (:TERMINALS (|x|)) (:PRECEDENCE NIL)
 (:START-SYMBOL |program|) (|program| (|x|)))


;;; Trivial grammars
(DEFINE-GRAMMAR TS-330 (:TERMINALS NIL) (:PRECEDENCE NIL)
 (:START-SYMBOL |program|) (|program| NIL))


;;; Web2c Report
(DEFINE-GRAMMAR TS-341 (:TERMINALS (|const_id_tok| |undef_id_tok| = |;|))
 (:PRECEDENCE NIL) (:START-SYMBOL CONST_DEC_PART)
 (CONST_DEC_PART (CONST_DEC_LIST)) (CONST_DEC_LIST (CONST_DEC))
 (CONST_DEC_LIST (CONST_DEC_LIST CONST_DEC)) ($@1 NIL)
 (CONST_DEC ($@1 |undef_id_tok| = |const_id_tok| |;|)))


;;; Web2c Actions
(DEFINE-GRAMMAR TS-342 (:TERMINALS (|else| |then| |const| |if|))
 (:PRECEDENCE NIL) (:START-SYMBOL |statement|)
 (|statement| (|struct_stat|)) (|struct_stat| NIL)
 (|struct_stat| (if else)) (if (|if| |const| |then| |statement|))
 (else (|else| |statement|)))


;;; Dancer 
(DEFINE-GRAMMAR TS-343
 (:TERMINALS (DATA STRING NUMBER INVALID ARROW > < |:| - +))
 (:PRECEDENCE NIL) (:START-SYMBOL |line|) (|line| (|header| |body|))
 (|header| (< |from| ARROW |to| > |type| |:|))
 (|header| (< ARROW |to| > |type| |:|)) (|header| (ARROW |to| |type| |:|))
 (|header| (|type| |:|)) (|header| (< >)) (|from| (DATA))
 (|from| (STRING)) (|from| (INVALID)) (|to| (DATA)) (|to| (STRING))
 (|to| (INVALID)) (|type| (DATA)) (|type| (STRING)) (|type| (INVALID))
 (|body| NIL) (|body| (|body| |member|)) (|member| (STRING))
 (|member| (DATA)) (|member| (+ NUMBER)) (|member| (- NUMBER))
 (|member| (NUMBER)) (|member| (INVALID)))



;;; --------------------------------------------


(defun run-test (grammar comp &rest options)
  (let ((report (second (multiple-value-list (apply #'make-parser (symbol-value grammar) :force-check t options)))))
    (format t "Test: ~a, result: ~s~%"
	    grammar
	    (equal
	     report
	     comp))
    report))

(defun test ()
  (let* ((s (make-string-output-stream))
	 (*error-output* s))
    (test_)
    (values)))

(defun test_ ()
(RUN-TEST 'TS-049
 '(:TERMINALS 14 :USED-TERMINALS 13 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 18 :USED-PRODUCTIONS 18 :STATES 31 :RESOLVED-CONFLICTS 42
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-101
 '(:TERMINALS 3 :USED-TERMINALS 3 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 4 :USED-PRODUCTIONS 4 :STATES 7 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-102
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 5 :USED-NONTERMINALS 2
   :PRODUCTIONS 10 :USED-PRODUCTIONS 4 :STATES 12 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 3 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-103
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 5 :USED-NONTERMINALS 3
   :PRODUCTIONS 10 :USED-PRODUCTIONS 5 :STATES 12 :RESOLVED-CONFLICTS 3
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-104
 '(:TERMINALS 1 :USED-TERMINALS 1 :NONTERMINALS 4 :USED-NONTERMINALS 3
   :PRODUCTIONS 5 :USED-PRODUCTIONS 3 :STATES 5 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 1))

(RUN-TEST 'TS-106
 '(:TERMINALS 6 :USED-TERMINALS 6 :NONTERMINALS 6 :USED-NONTERMINALS 6
   :PRODUCTIONS 9 :USED-PRODUCTIONS 9 :STATES 16 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 4 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-113
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 3 :USED-PRODUCTIONS 3 :STATES 4 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-114
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 9 :USED-NONTERMINALS 9
   :PRODUCTIONS 9 :USED-PRODUCTIONS 9 :STATES 11 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-115
 '(:TERMINALS 8 :USED-TERMINALS 8 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 8 :USED-PRODUCTIONS 8 :STATES 16 :RESOLVED-CONFLICTS 36
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-117
 '(:TERMINALS 11 :USED-TERMINALS 2 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 2 :USED-PRODUCTIONS 2 :STATES 4 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-119
 '(:TERMINALS 11 :USED-TERMINALS 2 :NONTERMINALS 11 :USED-NONTERMINALS 2
   :PRODUCTIONS 11 :USED-PRODUCTIONS 2 :STATES 4 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-121
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 4 :USED-NONTERMINALS 2
   :PRODUCTIONS 5 :USED-PRODUCTIONS 2 :STATES 6 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 1))

(RUN-TEST 'TS-124
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 8 :USED-PRODUCTIONS 8 :STATES 16 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-126
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 8 :USED-PRODUCTIONS 8 :STATES 19 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-128
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 8 :USED-PRODUCTIONS 8 :STATES 18 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-130
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 8 :USED-PRODUCTIONS 8 :STATES 22 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-132
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 9 :USED-PRODUCTIONS 9 :STATES 19 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-134
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 9 :USED-PRODUCTIONS 9 :STATES 25 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-136
 '(:TERMINALS 8 :USED-TERMINALS 8 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 10 :USED-PRODUCTIONS 10 :STATES 20 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 1))

(RUN-TEST 'TS-138
 '(:TERMINALS 8 :USED-TERMINALS 8 :NONTERMINALS 4 :USED-NONTERMINALS 4
   :PRODUCTIONS 10 :USED-PRODUCTIONS 10 :STATES 23 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-139
 '(:TERMINALS 3 :USED-TERMINALS 3 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 7 :USED-PRODUCTIONS 7 :STATES 9 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-144
 '(:TERMINALS 17 :USED-TERMINALS 17 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 17 :USED-PRODUCTIONS 17 :STATES 19 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-146
 '(:TERMINALS 9 :USED-TERMINALS 9 :NONTERMINALS 6 :USED-NONTERMINALS 6
   :PRODUCTIONS 13 :USED-PRODUCTIONS 13 :STATES 21 :RESOLVED-CONFLICTS 2
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-147
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 3 :USED-NONTERMINALS 3
   :PRODUCTIONS 4 :USED-PRODUCTIONS 3 :STATES 6 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 1 :REDUCE-REDUCE-CONFLICTS 0)
 :muffle-conflicts '(1 0))

(RUN-TEST 'TS-150
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 11 :USED-PRODUCTIONS 10 :STATES 18 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 2 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-151
 '(:TERMINALS 3 :USED-TERMINALS 3 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 3 :USED-PRODUCTIONS 3 :STATES 6 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 1 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-152
 '(:TERMINALS 3 :USED-TERMINALS 3 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 3 :USED-PRODUCTIONS 3 :STATES 6 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-153
 '(:TERMINALS 6 :USED-TERMINALS 6 :NONTERMINALS 3 :USED-NONTERMINALS 3
   :PRODUCTIONS 5 :USED-PRODUCTIONS 5 :STATES 11 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-154
 '(:TERMINALS 6 :USED-TERMINALS 6 :NONTERMINALS 3 :USED-NONTERMINALS 3
   :PRODUCTIONS 6 :USED-PRODUCTIONS 5 :STATES 13 :RESOLVED-CONFLICTS 1
   :SHIFT-REDUCE-CONFLICTS 1 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-155
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 4 :USED-NONTERMINALS 3
   :PRODUCTIONS 5 :USED-PRODUCTIONS 3 :STATES 6 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 1))

(RUN-TEST 'TS-157
 '(:TERMINALS 3 :USED-TERMINALS 3 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 3 :USED-PRODUCTIONS 3 :STATES 6 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 1 :REDUCE-REDUCE-CONFLICTS 0)
 :muffle-conflicts '(1 0))

(RUN-TEST 'TS-159
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 3 :USED-NONTERMINALS 3
   :PRODUCTIONS 4 :USED-PRODUCTIONS 4 :STATES 7 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 1)
 :muffle-conflicts '(0 1))

(RUN-TEST 'TS-162
 '(:TERMINALS 4 :USED-TERMINALS 4 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 4 :USED-PRODUCTIONS 4 :STATES 8 :RESOLVED-CONFLICTS 4
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

;; (RUN-TEST 'TS-165
;;  '(:TERMINALS 5 :USED-TERMINALS 4 :NONTERMINALS 7 :USED-NONTERMINALS 4
;;    :PRODUCTIONS 14 :USED-PRODUCTIONS 6 :STATES 16 :RESOLVED-CONFLICTS 5
;;    :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-185
 '(:TERMINALS 10 :USED-TERMINALS 10 :NONTERMINALS 12 :USED-NONTERMINALS 12
   :PRODUCTIONS 12 :USED-PRODUCTIONS 12 :STATES 22 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-322
 '(:TERMINALS 62 :USED-TERMINALS 59 :NONTERMINALS 46 :USED-NONTERMINALS 46
   :PRODUCTIONS 164 :USED-PRODUCTIONS 164 :STATES 320 :RESOLVED-CONFLICTS
   345 :SHIFT-REDUCE-CONFLICTS 65 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-322
 '(:TERMINALS 62 :USED-TERMINALS 59 :NONTERMINALS 46 :USED-NONTERMINALS 46
   :PRODUCTIONS 164 :USED-PRODUCTIONS 164 :STATES 2467 :RESOLVED-CONFLICTS
   2944 :SHIFT-REDUCE-CONFLICTS 265 :REDUCE-REDUCE-CONFLICTS 0)
   ;2620 :SHIFT-REDUCE-CONFLICTS 265 :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-325
 '(:TERMINALS 98 :USED-TERMINALS 76 :NONTERMINALS 124 :USED-NONTERMINALS
   124 :PRODUCTIONS 262 :USED-PRODUCTIONS 262 :STATES 443
   :RESOLVED-CONFLICTS 224 :SHIFT-REDUCE-CONFLICTS 78
   :REDUCE-REDUCE-CONFLICTS 10))

(RUN-TEST 'TS-325
 '(:TERMINALS 98 :USED-TERMINALS 76 :NONTERMINALS 124 :USED-NONTERMINALS
   124 :PRODUCTIONS 262 :USED-PRODUCTIONS 262 :STATES 10426
   :RESOLVED-CONFLICTS 15680 :SHIFT-REDUCE-CONFLICTS 1876
   :REDUCE-REDUCE-CONFLICTS 144)
 :canonical-lr1 t)

(RUN-TEST 'TS-328
 '(:TERMINALS 139 :USED-TERMINALS 139 :NONTERMINALS 46 :USED-NONTERMINALS
   46 :PRODUCTIONS 248 :USED-PRODUCTIONS 247 :STATES 426
   :RESOLVED-CONFLICTS 811 :SHIFT-REDUCE-CONFLICTS 0
   :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-328
 '(:TERMINALS 139 :USED-TERMINALS 139 :NONTERMINALS 46 :USED-NONTERMINALS
   46 :PRODUCTIONS 248 :USED-PRODUCTIONS 247 :STATES 4871
   :RESOLVED-CONFLICTS 7600 :SHIFT-REDUCE-CONFLICTS 0
   :REDUCE-REDUCE-CONFLICTS 0)
 :canonical-lr1 t)

(RUN-TEST 'TS-329
 '(:TERMINALS 2 :USED-TERMINALS 2 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 2 :USED-PRODUCTIONS 2 :STATES 4 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-330
 '(:TERMINALS 1 :USED-TERMINALS 1 :NONTERMINALS 2 :USED-NONTERMINALS 2
   :PRODUCTIONS 2 :USED-PRODUCTIONS 2 :STATES 3 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-341
 '(:TERMINALS 5 :USED-TERMINALS 5 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 6 :USED-PRODUCTIONS 6 :STATES 11 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-342
 '(:TERMINALS 5 :USED-TERMINALS 5 :NONTERMINALS 5 :USED-NONTERMINALS 5
   :PRODUCTIONS 6 :USED-PRODUCTIONS 6 :STATES 12 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))

(RUN-TEST 'TS-343
 '(:TERMINALS 11 :USED-TERMINALS 11 :NONTERMINALS 8 :USED-NONTERMINALS 8
   :PRODUCTIONS 24 :USED-PRODUCTIONS 24 :STATES 42 :RESOLVED-CONFLICTS 0
   :SHIFT-REDUCE-CONFLICTS 0 :REDUCE-REDUCE-CONFLICTS 0))
)

