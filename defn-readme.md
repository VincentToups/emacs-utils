A Guided Tour of an Implementation 
==================================
of Clojure Destructuring Bind
=============================
in Emacs Lisp
-------------

### Introduction ###

Clojure is a new (relatively) Lisp variant which has attracted a lot
of attention due to its modern features and close relationship with
the JVM.  Emacs Lisp is a lisp so antiquated that it attracts more
ridicule than plaudits.  This document describes an attempt to bring
some of the nicer syntactical (and one semantic) features from Clojure
into Elisp.

### Destructuring Bind ###

Clojure owes a certain debt to the statically typed functional
languages.  It's emphasis on Lazy values references languages like
Haskell, and is destructuring bind forms refer to the pattern matching
(efficiently) afforded by static type systems like those in Standard
ML and its variants.  Destructuring bind has, of course, appeared in
places like Lua and Python, where simple tuples can be destructured,
but Clojure's support is somewhat more extensive in that it supports
generic destructuring of sequences and tables.  In Scheme (for
instance) we might wish to swap two numbers held in the first and
second slot of a list.

    (define swap (lst) 
     (list (cadr lst) (car lst)))

In Clojure we could specify how to extract values from the arguments
of a function right in the function definition:

    (defn swap [[a b]] (list a b))

(Clojure's binding forms are by convention represented by vectors.)
Here SWAP takes only one argument, but it binds two names.  The inner
`[a b]` expression indicates that the single argument is expected to
be a list, and that its first and second values should be bound to the
symbols `a` and `b` respectively.   Clojure's destructuring bind
supports recursive binding forms.  If, for instance, we wanted to
write a function which accepts a single list, whose second element is
a list, whose first value we wish to extract, we could write:

    (defn extract [[_ [a]]] a)

Where we have nested the destructuring deep into the sequence to pull
out `a`.  Error checking aside, this is a pretty nice feature,
particularly because Clojure _also_ allows destructuring on tables,
which have their own source-level representation.  To write a function
which pulls out the values in a table located at keys `:x` and `:y`
and returns them in a flat list, we can write:

    (defn extract [{a :x b :y}] (list a b))

Suppose the value at `x` were a list and we wished to get its second
element:

    (defn extract [{[_ part] :x}] part)

Would do the trick.  We can combine, recursively, table and sequence
destructuring syntax.  This project is an implementation of this
feature in Emacs Lisp.

### Bonus Material: Recur ###

Tail call optimization is a controversial subject in some arenas.  The
feature was somewhat famously kept out of Python by its "dictator for
life."  Scheme implementations, on the other hand, are required to
suppor this feature.  Clojure takes a middle path (more for reasons
related to the JVM than any political sensitivity): tail calls to
"oneself" can be made by virtue of an explicit form, `recur`, which
resembles a function call but can only be invoked from tail position,
and which reuses the current stack frame instead of creating a new
one.  This allows many basic algorithms which depend on tail calls for
elegant expression to be written naturallly in Clojure.

This library also allows the use of a `recur` special form, statically
checked to be only from tail position.  

### Syntactic Notes & Front Matter ###

Clojure supports tables at the level of source code via a curly braces
notation:

    {:x 10 :y 11} 

Would be the table with keys `:x`, `:y` pointing to 10 and 11
respectively.  Unfortunately, Emacs Lisp does not provide facilities
to extend the reader, which would could use to create a syntax for
tables if we were using (say) Common Lisp.  We'll be using a mildly
ad-hoc solution.  My standard library provides functions to create
tables succinctly:

    (tbl! :x 10 :y 11)

Is equivalent to the above Clojure.  For destructuring we will use a
vector to represenent sequences (`[a b c]`) and a vector with a
special head token to represent tables (`[:: a :x b :y]`).  That is,
`::` indicates an expression represents a table, rather than sequence
destructuring.  

Additionally, we'll allow the table syntax to destructure
association-lists, since these are common table surrogates in other
lisp dialects and because they can be made persistant (as data
structures) more easily than Emacs Lisp's tables.  

Finally, this implementation uses a few non-standard special forms
from my standard library which bear remarking upon.  The form
`let-seq`, defined in `utils.el` is a simple form for destructuring
lists.  It creates a context where a series of symbols are bound to
the values in a list:

    (let-seq (a b c) (list 3 2 1) 
      (list a b c))

Evaluates to '(1 2 3). 

The form `let-tbl` allows a very simple form of table destructuring.  
    
    (let-tbl 
     ((x :a)
      (y :b))
     (tbl! :a 10 :b 11)
     (+ x y))

Evaluates to 21.  The implementation makes use of other functions in
the `utils.el` library, but these are the most conspicuous departures
from recognizeable lisp.


 

