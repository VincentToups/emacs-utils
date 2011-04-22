Clojure-like Emacs
------------------

(NOTE: changes to the monad library regarding scope have broken tons
of code (temporarily) !)

This is a group of loosely organized utilities to add clojure like
abilities to emacs-lisp.

It consists of defn and fn forms for declaring functions and anonymous
functions.  These forms support the usual clojure-style destructuring
bind and arity based dispatch on function arguments.  Another macro,
dlet, implements destructuring bind for "let".  All of them are
lexically scoped, rather than the emacs default dynamic scope.
Despite this, a defn function is an emacs function, and can be
declared interactive.

defn, fn, dlet examples
-----------------------

These work in a manner similar to the clojure equivalents.

    (require 'defn)

    (defn f 
      ([x] x)
	  ([x y] (* x y)))
    (f 10) ; -> 10
    (f 10 20) ; -> 200

This defines a function of one or two arguments.  When the function is
called, the correct body is invoked based on arity.

While emacs lacks a special syntax for tables, it does have hash
tables.  This library supports destructuring bind on hash-tables also
via the following syntax:

    (defn f [a 
		     [:: z :z :or (tbl! :z 100)]] 
          (list a z))

    (f 10 (tbl! :z 11)) -> (10 11)
    (f 10 (tbl!)) -> (10 100)


Note that `(tbl! :z 10)` is short hand which creates a hash table.  It
can take multiple arguments: `(tbl! :x 10 :y 11 :z 14)`.  Quick access
to the values in the table is accomplished via `tbl`: `(tbl
a-table:x)` gives the value stored by `:x`.  These functions are
included in the library.

Destructuring bind is fully recursive, so you can nest desctructuring
syntax deeply and the macros will do the right thing.  The macro
`dlet` works as you expect:

    (dlet [x 10 y 11] (+ x y)) ;-> 21

All of them create lexical scopes.  An alternative set of forms create
dynamic scopes instead.  These are available as `fn_`, `defn_` and
`dlet_`.

Functions and the `dloop` and `dloop_` forms create recursion points
so that the `recur` form causes a non-stack increasing recursion.
This is implemented via a codewalking macro and the new `dsetq` form,
which takes the same kind of input as a let, but setqs the values
instead of creating a scope.  With these extensions you can write:

	(dloop [x 0 output nil] 
		   (if (> x 10) (reverse output)
		   	   (recur (+ x 1) (cons x output))))

Which evaluates to '(0 1 2 3 4 5 6 7 8 9 10).  Functions create
implicit loop points, so you can implement the product function like
so:

    (defn prod 
      ([[val & rest :as lst] acc]
       (if (not lst) acc
 	     (recur rest (* val acc))))
      ([lst]
       (prod lst 1)))

Recur does not cause stack to be consumed.  It compiles into the loop
macro.  Anonymous functions also support `recur`.

Monads!
-------

I've implemented primitive monad support using this library.  These
monads are patterned after the clojure contrib monad implementation
(although they are substantially less complete).  Monads use the same
syntax for binding as clojure, so you can do neat things with them.

A simple example is the Identity monad.

    (require 'monads)

    (domonad monad-id [x 1 y 2] (+ x y)) ;-> 3

Slightly more interesting is the Maybe monad.

    (domonad monad-maybe 
       [x (Just 20) 
        k (maybe/ x 4) 
        y (maybe+ k 1)] 
      y) ;-> (Just 6)

But:

    (domonad monad-maybe 
       [x (Just 20) 
        k (maybe/ x 0) ; Divide by zero ruins the calculation
        y (maybe+ k 1)] 
      y) ;-> (None)

Even more interesting is the sequence monad:

    (domonad monad-seq 
       [x (list 1 2 3) 
        y (list 4 5 6)] 
      (list x y)) ;-> ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))

Note that destructuring bind works with the monads:

    (domonad monad-seq
       [[a b] (list '(1 2) '(3 4) '(5 6))]
      (+ a b)) ;-> (3 7 11)

This is built on top of the fn implementation, so all the
desctructuring supported there works here.

The implementation is wacky right now, but will be cleaned up when I
can find moments of respite from writing my dissertation.
  
Updates:
--------
Update 19 Apr 2010

* fixed a bug in the expansion of recur wherein tail-calls inside
  let-like forms (let, let*, lexical-let, lexical-let*, labels, flet)
  would not expand.
* Improved the fn and fn_ macros so that they do not compile to loops
  if recur is not used in their bodies.  This is a tradeoff between
  compilation and execution speed.

Update 16 Mar 2010

* finally implemented recur keywords.  Also implemented a
  destructuring set operation.  Added a dloop macro.

Update 28 Aug 2009

* added State Monad (monad-state).

Update 26 Aug 2009

* fixed a regression in defn code which disabled the ability to make defn's interactive.
* ADDED MONADS!!!
  * identity monad
  * maybe monad
  * sequence monad
* Monads support clojure-style destructuring bind via domonad expressions.
* no support for with-monad yet.


Update 17 July 2009

* fixed bug in dlet which caused an error with using the let*-like semantics.  dlet is now a recursive macro
* todo: re-implement fn in terms of dlet.  Still will require special handling of the top-level form.

Update 15 July 2009

* fixed bug wherein empty arg-lists would give an error.

Update 9 June 2009
* added support for :or forms for sequence binding types.
* Note: You can't use them at the top-level binder because they would confuse the automatic dispatch of function bodies on arity.  Sorry.

Update 8 June 2009
* added support for :keys in table binder, corrected several small bugs introduced by new parsing code.

Update 7 June 2009
* rewrote binder parsing code so that it is easier to extend and support error checking
* added support for :or forms in table binders.  The form after :or is evaluated at call-time, rather than compile time, and must result in a hash-table with the appropriate keys to provide defaults.

(defn f [a [:: z :z :or (tbl! :z 100)]] (list a z))
(f 10 (tbl! :z 11)) -> (10 11)
(f 10 (tbl!)) -> (10 100)


Update 2 June 2009
* added extensive checking of binder forms at compile time and what are hopefully informative error messages. 
* Todo
  - add support for :or form to the table destructuring

Update 1 June 2009
* defn now expands in terms of the fn macro
* lots more error checking.  Some simple checks for misformed binders, also checks for arity when the function is called.

Update 31 May 2009
* added support for multiple arity defn and fn definitions:

    (defn f 
      ([x] x)
	  ([x y] (* x y)))
    (f 10) ; -> 10
    (f 10 20) ; -> 200

  

Some emacs utilities with clojure-style destructuring bind.

	 (defn demo [a b [c d]] (list a b c d))
	 (demo 1 2 (list 10 11)) -> (1 2 10 11)

The binding also works with hashtables.

	(defn demo [a b [:: c :x d :y]] (list a b c d))
	(let ((a-table (tbl! :x 100 :y 110)))
	  (demo 1 2 a-table)) -> (1 2 100 110)

Most useful in the utils package may be a pair of functions for creating and manipulating hash tables.

	 (tbl! :x 10 :y 11) ; creates a hash table with keys :x, :y and associated values
	 (tbl a-table :x) ; returns the value at key :x

Update 5 June 2010
* Corrected a bug in defn expansion with recur.

Update 10 Oct 2010
* added lots of bug fixes, codewalking macros and functions to help
write them, `capturing-defun` and `capturing-lambda` which attempt to
meaningfully capture their apparent lexical scope (don't really work)
* added lex-lambda and lex-defun which automatically create lexical
scopes around their bodies, containing their args.
* implemented monadic parser combinators using the monads in monads.el
* added lots more stack language functions.
* added a units library for dealing with SI units sort of smartly,
integrates with with-stack, because unit calculations look nice there.
* partial support for an interface to [animator](http://repo.hu/projects/animator/) in `animator.el`.