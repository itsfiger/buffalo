# buffalo
A canonical LR(1) and LALR(1) parser generator in Common Lisp.

Buffalo is a parser generator: it computes parsing tables and
associated semantic actions out of a BNF grammar specification that
are then used by a driver (also part of the package) to parse input in
the language specified by the grammar.

Buffalo's LR(1) generator is based on Knuth's original canonical
method, creating a full LR(1) automaton (without state merging), while the
LALR(1) generator is an implementation of the DeRemer/Pennello
algorithm to compute lookahead sets.

buffalo implements full POSIX yacc-compatible conflict resolution
mechanisms, including per-production context-dependent precedence
(%prec).

Both generators use sparse sets, patricia tries, and other efficient
and scalable algorithms, to achieve very competitive compile times, in
particular noticeable for the LR(1) method and large grammars.

Compile times of typical grammars like the accompanying C grammar
should be fractions of a second for both the LALR(1) and LR(1) method,
e.g. with sbcl they are:

Grammar       | LALR          | States         | canonical LR(1) | States
------------- |:-------------:|:--------------:|:---------------:|:---------:
C             | 0.04s         | 400            | 0.24s           | 1856
Ada           | 0.09s         | 861            | 0.46s           | 12787

Contrary to what the literature suggests, LR(1) machines have *not*
been found to be exponentially larger but only by an order of
magnitude or so, rendering them a worthwhile alternative (proofs of the
opposite are welcome!).


## Features Summary

- Very fast, scalable table construction
- Simple compilation model for seamless integration with Lisp
- Comprehensive grammar checks
- Basic Export/Import facilities to and from POSIX yacc
- yacc-style conflict resolution mechanism
- Conflict explanation

Please note that buffalo does need a token generator ("lexer"), that
is currently not bundled with buffalo, to do its work.


## Installation and basic usage

Buffalo is quickload-able and asdf-loadable and is tested with sbcl,
ccl and clisp, but should work with every conformant
implementation. The version in quicklisp might not reflect the latest
changes, though. Depending on your implementation, asdf might have to
be loaded first

```lisp
(require 'asdf)
```

buffalo can then be installed either by

```lisp
(require 'buffalo)
```

or by

```lisp
(asdf:load-system 'buffalo)
```

depending on your implementation. Please consult the asdf
documentation when unsure about search paths for the systems.

Parsers are defined via a widely used, list-based BNF-like syntax,
where a non-keyword symbol starts a production, followed by a list of
right-hand sides, being either a single symbol or a list of symbols
that represent terminals or non-terminals:

```lisp
(define-parser my-first-parser
    (:terminals (+ * id))
    (:start-symbol s)
    (s
     (s + m #'plus)
     m)
    (m
     (m * id #'times)
     id))
```

The last argument in a list is the semantic action that defaults
to "identity" in the case of a single symbol. Please consult the
manual for all the gory details.

In addition, buffalo implements all of the functionality and all of
the user interface functions and macros of
[cl-yacc](https://github.com/jech/cl-yacc) (on whose
API code it is in part built). Software that runs with cl-yacc should
work with buffalo without modification (buffalo might have some feature
that cl-yacc has not, though).


## Examples

### Buffalo-tests

This package contains various small grammars (right- and left-recursive,
ambiguous, with left or right precedence or non-associativity, of LALR(1) or
LR(1) type).

### Conflict explanation

buffalo is able to explain conflicts in states by computing sentences
of nonterminal and terminal symbols that lead (via a shortest path) to
the conflicting state.

For the accompanying C grammar, typing

```lisp
(make-parser cgrammar :force-check t :canonical-lr1 t)
```

produces

```
WARNING: Conflict explanation:
Derived sentence for state 1618:
Sentential form:
DECLARATION_SPECIFIERS DECLARATOR   "{" "IF" "(" EXPRESSION   ")" "IF" "(" EXPRESSION   ")" STATEMENT . :ELSE
Terminal form:
"CONST"                "IDENTIFIER" "{" "IF" "(" "IDENTIFIER" ")" "IF" "(" "IDENTIFIER" ")" ";"       . :ELSE

Derived sentence for state 1701:
Sentential form:
DECLARATION_SPECIFIERS DECLARATOR   "{" "DO" "IF" "(" EXPRESSION   ")" "IF" "(" EXPRESSION   ")" STATEMENT . :ELSE
Terminal form:
"CONST"                "IDENTIFIER" "{" "DO" "IF" "(" "IDENTIFIER" ")" "IF" "(" "IDENTIFIER" ")" ";"       . :ELSE
```

showing that the grammar is ambiguous due to the "dangling else" problem (which
is solved by a default shift action).

### Parser factory

It shows how semantic actions can be modified programmatically
even though the parser is defined by a macro at compile time:

```lisp
(defun parser-factory (plus times)
  (def-parser
    (:terminals (+ * id))
    (:start-symbol s)
    (s
     (s + m (values plus))
     m)
    (m
     (m * id (values times))
     id)))

(defun test-parser-factory ()
  (let ((list '(id + id * id)))
    (flet ((list-lexer (list)
	     (lambda ()
	       (let ((x (pop list)))
		 (values x x))))
	   (plus (a p b)
	     (declare (ignore p))
	     `(+ ,a ,b))
	   (times (a p b)
	     (declare (ignore p))
	     `(* ,a ,b)))
      (let ((parser-a (parser-factory #'list #'list))
	    (parser-b (parser-factory #'plus #'times)))
	;; print as infix: > (ID + (ID * ID))
	(print
	 (parse-with-lexer (list-lexer list) parser-a))
	;; print as prefix: > (+ ID (* ID ID))
	(print
	 (parse-with-lexer (list-lexer list) parser-b))))))
```

### Test Suite

All applicable tests of the GNU Bison Test Suite have been
translated.

For license reasons they have been put into a separate project
[buffalo-testsuite](https://github.com/fhuttner/buffalo-testsuite).

The so called "torture tests" demonstrate that buffalo can build
parsers for grammars with many thousand tokens and productions,
and in time and space requirements comparable to GNU Bison.


## Literature

[1] Alfred V. Aho, Ravi Sethi and Jeffrey D. Ullman. Compilers: Principles, Techniques, and Tools. Addison-Wesley, 1996.

[2] Seppo Sippu and Eljas Soisalon-Soininen. Parsing Theory, Vol. I. Springer, 1990.

[3] Robert Sedgwick. Algorithms in C++, Parts 1-4. Addison Wesley Longman, 1999.

[4] DeRemer, Frank, and Thomas Pennello. "Efficient computation of LALR (1) look-ahead sets." ACM Transactions on Programming Languages and Systems (TOPLAS) 4.4 (1982): 615-649.

[5] Park, Joseph CH, Kwang-Moo Choe, and C. H. Chang. "A new analysis of LALR formalisms." ACM Transactions on Programming Languages and Systems (TOPLAS) 7.1 (1985): 159-175.

[6] Fancois Pottier and Yann Regis-Gianas. Menhir. http://cristal.inria.fr/fpottier/menhir

[7] Davis, Timothy A. Direct methods for sparse linear systems. Vol. 2. Siam, 2006.
