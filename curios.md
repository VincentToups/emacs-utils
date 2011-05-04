A Quick Overview of the Curios in this Library
==============================================

Clojure-Style Destructuring Bind/Function Definitions
-----------------------------------------------------

This library, `defn.el`, supports an approximation of Clojure's
destructuring bind syntax.  If you are familiar with Clojure, the
following code will be recognizable:

    (require 'defn)
    (defn prod 
      ([[el & tail :as lst] acc]
       (if lst (recur tail (* el acc))
           acc))
      ([lst] (prod lst 1)))
    (prod '(1 2 3 4)); -> 24

The above code demonstrates the declaration of a function, `prod`
which has two arities: the number of arguments determines which body
is executed.  If one arg is passed in, prod calls itself with the
appropriate initial value for its accumulator.  If two arguments are
provided, it uses a `recur` expression to call itself repeatedly,
calculating the produce of a list passed in.  This is done without
growing the stack.  You can recur within a single body perpetually.  

`defn` and its anonymous cousin `fn` support destructuring on hash
tables and association lists where each element in the list is a
proper list:

    (defn dst-demo [[:: x :x y :y]]
      (list x y))

    (dst-demo (tbl! :x 10 :y 11)); -> (10 11)
    (dst-demo '((:x 40) (:y 50))); -> (40 50)

Default values may be provided for both sequential and table forms
using the `:or` keyword (clojure doesn't support `:or` for sequences).
Destructuring can be nested arbitrarily deeply as well.  There are
also `let` forms which provide the same destructuring for `let`-like
situations.  All these forms expand to lexical-closures, but there are
forms like `defn_` which are identical but create dynamic scopes
instead.

As with most heavy macro magic, you can't really use this library
without byte compiling, but when this is done, it can be reasonably
zippy. 

Standalone Recur
----------------

I really like the `recur` feature from Clojure.  I like it so much I
factored it out of the `defn` implementation so that it can be used in
situations where a lighter touch is required.  

    (require 'recur)
    (recur-defun* prod (lst &optional (acc 1))
      (if (empty? lst) 
          acc
          (recur (cdr lst) (* acc (car lst)))))

Again, this won't blow the stack.  The lambda list supports the whole
`cl` style lambda list.  Also provided is a `let` form which can recur
on itself, `recur-let`.

Multi-methods
-------------

Another Clojure-inspired feature is multi-methods.  These are a kind
of proto-multiple dispatch "object" system, by which I mean they are
slightly more general than any concrete object system.  You can use
them to implement a single-dispatch object system-like functionality,
for instance:

    (require 'multi-methods)
    (defmulti volcalize-n-times (lambda (&rest args)
                              (pseudo-class-of (car args)))
      "Volcalization multi-method.") ;Must be executed before defunmethod

    (defunmethod volcalize-n-times :cat (thing n)
      (loop for i from 1 to n do (print "Meow")))

    (defunmethod volcalize-n-times :dog (thing n)
      (loop for i from 1 to n do (print "Woof")))

    (defunmethod volcalize-n-times :human (thing n)
      (loop for i from 1 to n do (print "Lorem ipsum dolor sit
      amet, consectetur adipiscing elit. Nulla sed sapien ligula. Sed
      luctus cursus consequat. Morbi egestas magna auctor dui
      sagittis bibendum. Ut in felis sit amet eros eleifend ultricies
      gravida id ligula. Suspendisse potenti. Nulla semper porttitor
      massa, sed feugiat mauris tempor nec.")))

    (defun pseudo-class-of (val)
      (gethash :pseudo-class val :thing))

    (volcalize-n-times (tbl! :pseudo-class :cat :name 'tibald) 10)
    ; prints "meow" 10 times.
    (volcalize-n-times (tbl! :pseudo-class :human :name 'vincent) 2)
    ; prints lorem-ipsum fragment twice.

Multimethods match their arguments via `isa?` predicate, which can be
extended with functions like `derives`.  For instance:

    ($ :cat derives-from :quadraped)
    ($ :dog derives-from :quadraped)
    ($ :human derives-from :biped)
    ($ :emu derives-from :biped)

Note that `$` is a simple infix macro that swaps the function position
with the first argument position in the top level of the subsequent
forms.

    (defmulti n-legs 
	  (lambda (&rest args)
	   (pseudo-class-of (car args)))
      "Number of legs multi-method")
   
    (defunmethod n-legs :biped (o) 2)
    (defunmethod n-legs :quadraped (o) 4)

    (n-legs (tbl! :pseudo-class :human)) ;-> 2
    (n-legs (tbl! :pseudo-class :cat)) ;-> 4

By writing the appropriate dispatch function you can create many kinds
of crazy object systems.  Dispatch is cached, so if the `derives`
hierarchy doesn't change, dispatch is fast after the first method
call.

Partial Application
-------------------

Particularly because emacs lisp is dynamically scoped, its handy to
have functions to create functions which handle lexical static for
you.  If you `(require 'functional)`, you'll have access to these
functions, which make point free programming more fun:

    (mapcar (par #'+ 5) '(1 2 3));-> (6 7 8)

`par` partially applies a function on the right.  `pal` does the same
on the left.  Both always return a function, so you can partially
apply more arguments than a function has, in which case when you call
the result, you'll get an error.  Considering that elisp functions
have variable and unlimited arity, this seemed like the most idiomatic
accomidation.  There are also some other interesting things in
`functional.el`, though it probably should be called `point-free.el`.

With-stack
----------

`with-stack.el` is an emacs lisp embedded stack language in the mode
of Factor or Joy.  It looks like this:

    (||| 4 4 + ) ;-> 8

The macro `|||` introduces a stack language form.  Subsequent objects
are interpreted as in a stack languages, with atomic forms pushing
themselves onto the stack and symbols acting as "words" which
manipulate the stack.  The file `stack-words.el` defines many basic
stack words, but you can define your own using the `word:` word.

    (||| word: plus-four 4 + end: 10 plus-four ) ;-> 14

`|||` returns whatever is on the top of the stack at the end of
evaluation.  The stack is discarded between invocations of `|||`.  The
language supports the notion of regular "words" and parsing or
immediate words, which are executed when they are encountered during
the compilation scan of the stack language segment.  An immediate word
is written in the stack language itself, and sees the "future" stream
of tokens as its stack.  It may manipulate that stack as it sees fit
before terminating, at which point, compilation continues till the
next immediate word or the end of the stack segment.

    (||| immediate-word: -lisp-val: '1>eval swap end:)
    (||| -lisp-val: '(+ 1 1) ) ;-> 2

The language supports Factor/Joy style quotations and `stack-words`
defines the most common combinators (`if`,`loop`,`bi`, etc).  You can
call emacs functions using a bit of sugar (shown above).  The syntax
`2>concat`, for instance, means "call concat with two values from the
stack."  These glue-words are assumed to return 1 value, so don't
forget to `drop` nils.

Conclusion
----------

That is a bunch of the crazy stuff I have been working on.  There are
other projects in a half-way state of completion.  All of these
projects are on my github, as is this document.  They depend on my
giant pile of utils library `utils.el`.

