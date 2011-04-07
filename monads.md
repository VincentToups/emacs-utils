Advanced Elisp Part 1
=====================
Implementing Monads
-------------------

I've started working on an implementation of Kanren, the logic
mini-language covered in [The Reasoned Schemer][trs], in Elisp.  I
want to write about this process visa-vi the unique considerations of
Emacs Lisp as a language, but its a middle sized project, which
depends on several pieces of code, which themselves depend on code
which I haven't described in detail anywhere.  Kanren depends on a
monad (the "stream of substitutions" monad, for the nerds out there).
An implementation of this monad is a good place to start when thinking
about implementing Kanren itsef, but I've never written up a piece of
documentation on the implementation of Monads I've cooked up for
Elisp, so I thought I'd start there.

First, credit where its due: the following implementation of Monads
owes is basic form to the [monad library in
Clojure-contrib][cc-monads].  I wrote it while reading through Jim
Duey's completely excellent [monad tutorials for
clojure][cljm-tut].


Besides minor considerations associated with dealing with the default
dynamic scope in Elisp, the implementation here is basically that of
the Clojure Contrib library.  And I could never have understood what I
was doing without the tutorial.  

### Onto the Monads ###

We've reached the monad tutorial part of this document.  I read tons
of tutorials when I was trying to wrap my mind around monads, and I
can't say any one specific one really did the job.  I really had to
implement monads in Elisp to finally get it, and then it only really
clicked after I constructed a few "novel" monads from scratch (see
[these][pryor-notes] course notes).  I'm pretty sure,
after all this that the best way to get monads is to just start using
them and thinking about them a lot.  But we'll give it a go anyway.  

I think of a monad as a way of decorating function composition.  If I
have a group of functions that conform to a particular input and
output constraint, a monad associated with that constraint tells me
how to combine those functions so that something interesting happens.
The fact that these functions all have the same type of input and
output is critical, otherwise the "plumbing" specified by the monad
won't work.  Such functions are called "monadic functions" and when we
try to understand monads, or a specific monad, we must meditate on the
type of these functions.

Lots of people try to understand monads coming from a dynamic language
perspective, where the types of functions are not often considered
carefully.  By type we mean the information about what kinds of things
go into a function and what kinds of things leave it.  Monadic
functions always have a recognizeable type: they take a single
argument and return a monadic value.  What that value is depends on
the monad, but for a given monad, all monadic functions will have the
same return type.  In most monads, one can consider the monadic value
as "containing" the input type in some way.

We'll be talking about the sequence monad first.  We will represent
sequences with regular old lisp lists, which can hold values of any
lisp type.  The monadic type, then, is lists.  Lists "contain"
(really, they literally contain) lisp data.  Let us meditate upon the
type of monadic functions in the sequence monad.

==== this space reserved for meditation ==== 


If you guessed in your meditation that the monadic functions
associated with the sequence or list monad (we'll use the two
interchangeably from here on) are functions which take a single value,
which is any lisp data, and return a list of lisp-values, you got it.
So, whatever the hell a "list monad" is, its got something to do with
composing functions which take a single lisp value and return a list.  

Lets start kicking around ideas about composing these functions.  To
do that we need a few functions to consider explicitely.  What are the
simplest monadic functions of the list monad?  Well `list` is
certainly such a function when you call it with one argument, so lets
put that on the pile.  In that spirit, consider:

    (defun stub (x) nil)
    (defun list-return (x) (list x))

`stub` is also a monadic function - it ignores its inputs and returns
the empty list.  Let's consider a couple more, for variety:
    
    (require 'cl)
    (defun list-incr (x) (list (+ x 1)))
    (defun list-decr (x) (list (- x 1)))
    (defun list+/- (y) (lexical-let ((y y))
       (lambda (x) (list (+ x y) (- x y)))))

The functions `list-incr` and `list-decr` are obvious enough.  That
third beast is not itself a monadic function, but it _returns_ a
monadic function whose behavior is partially specified via lexical
closure over the variable `y`.  This is a very common pattern, and
we'll see how it works in a bit.

But we were meditating on function composition.  Lets just to just
compose these functions directly.  To do that we should write a
function which composes functions.


    (defun compose2 (f2 f1)
      "Compose F2 and F1 by returning a new function which
       calls F1 on its arguments, then calls F2 on the result"
       (lexical-let ((f1 f1)
                     (f2 f2))
        (lambda (&rest args)
          (funcall f2 (apply f1 args)))))

    (defun compose (&rest fs)
      "Compose the functions FS (FN FN-1 FN-2 ... F1) 
       beggining with F1 and working backwards."
      (let ((rfs (reverse fs)))
       (reduce
         (lambda (acc-f f)
           (compose2 f acc-f))
         (cdr rfs)
         :initial-value (car rfs))))

Lets try just composing some of our list monadic functions.  

    (compose #'list-incr #'list-decr)

Because this is Elisp, we don't have any obvious problems so far.  Of
course, if we think about this, we know it can't work.  `list-decr`
takes a value, subtracts one, puts it in a list.  Then we try to call
`list-incr` on that value, but it will barf, because it will try to
add one to a _list_, which is not defined.  Try it

    (funcall (-compose #'list-incr #'list-decr) 10)

My emacs gives:

    Debugger entered--Lisp error: 
        (wrong-type-argument number-or-marker-p (9))
      +((9) 1)

Suppose we were dead set on jamming these two functions together and,
in particular, we want to make sure that the resulting function can be
used wherever either of the two functions might have been used.  That
is, we want the result type to be the same as the input types.  There
are several approaches, obviously, but here is a sort of obvious one
(read the comments here):

    (defun list-func-compose (f2 f1)
       (lexical-let ((f1 f1)
                     (f2 f2))
        (lambda (arg) ; lambda need take only a single arg, as f1 must
          (let* ((r1 (funcall f1 arg)) 
            ; r1 is a list, because its the result of f1, a monadic function.
            ; f2 accepts regular old values, though, and r1 could
            ; concievably have many values in it.
                 (r2s (mapcar f2 r1)))
              ; now r2s contain the results of applying f2 to ALL
              ; the values in r1.  Each of these values in r2s is
              ; itself a list, because f2 always returns a list (it 
              ; is a monadic function itself)
                 (reduce #'append r2s)
              ; Well, we want a list of things in the monad, not a
              ; list of lists, so we just append all the r2s together 
              ; (see footnote #1).
              ; Now we've got a list of lisp values, which  means
              ; that this lambda is a monadic function in its own
              ; right.  Success!!
              ))))

(n.b. If something about `r2s` type strikes you as odd, see footnote 1.)

I hate when, in the course of a didactic development, something
springs from nowhere with the proviso that "it will be useful later."
This kind of thing is typical of physics and math texts, and I think
its part of the reason they are so difficult to read.  However, for
the sake of future utility, we are going to factor out a piece of the
above code.  Consider the function `list-bind` which takes a monadic
value and a monadic function and returns a new monadic value:

    (defun list-bind (mon-val mon-fun) ; -> mon-val 
           (let ((results (mapcar mon-fun mon-val)))
              (reduce #'append results)))

We can rewrite `list-func-compose` like this, now:

    (defun list-func-compose (f2 f1)
      (lexical-let ((f1 f1)
                    (f2 f2))
        (lambda (value) 
           (let ((monadic-value (f1 value)))
              (list-bind monadic-value f2)))))

Ok, what is so special _a priori_ about `list-bind` (_post priori_,
its one of the monad functions which any monad needs)?  `List-bind`
is, in a way, the fundamental operation relating monadic functions and
monadic values in the list monad.  `List-func-compose` might seem more
directly useful, but it is kind of strange that the resulting function
takes a non-monadic value and returns a monadic one.
`List-func-compose` relates monadic functions and other monadic
functions, but it doesn't reveal much about how they are combined with
values.  `List-bind` describes that process, and it turns out this is
more interesting.

Ok, believe it or not, we've basically covered the entire sequence
monad already.  All that is left is to really get the idea and to
understand how it relates to Haskell's do notation and the canonical
monad operations.

Let's try things out, however.  

    (funcall (list-func-compose #'list-decr #'list-decr) 5); -> (3)

Pretty anti-climactic, for sure.  But we can do more interesting
things:

    (funcall (list-func-compose
              (lambda (x) (list (+ x 1) (- x 1)))
              (lambda (x) (list (+ x 3) (- x 3))))
             0); -> (4 2 -2 -4)

Which gives us all the combinations of adding and subtracting three to
the initial value.  We could also write this more succintcly as:

    (defun list+/- (n) (lexical-let ((n n))
           (lambda (x) (list (+ x n) (- x n))))) ;(remember this guy?)
    (funcall (list-func-compose
              (list+/- 1)
              (list+/- 3))
            0) ; -> (4 2 -2 -4)

Which is a pretty common pattern.  `List+/-` produces a parameterized
monadic function which we customize to our needs as we go.
`List-func-compose` captures the essence of the list monad, but its
not in its usual form.  We'll come to that in a bit.

### Stepping Back ###

Monads in one sentence: a monad is method for composing a class of
functions with a specific type into a single function with that type.
The method differs from monad to monad, but the meat of the monad is
in the extra compositional plumbing the monad adds.

Everything else is window dressing.

### But How Nice The Window Dressing ###

Let's talk about `let*`.  We've been twisting up our brains into knots
over monads.  Let those knots untwist and return to this simple,
clean, construct.  In Lisp, variable bindings are explicitely
introduced.  You don't just declare a variable and go, you create a
context for that variable with a `let` or `let*` statement.  You
probably know that if you only had `lambda` and `defmacro` you could
define `let` like so:

    (defmacro my-let (bindings &rest body)
      (funcall (lambda ,(mapcar #'car bindings)
                 ,@body)
          ,@(mapcar #'cadr bindings)))

eg:

    (my-let ((x 1) (y 2)) (+ x y)) 

expands to

    (funcall (lambda (x y) (+ x y)) 1 2)

It is clear that x and y can't depend upon one another in this form:

    (funcall (lambda (x y) (+ x y)) 1 (+ x 2)) 

Obviously won't work (why?).  `let*` is the form which lets us _chain
together_ multiple variable bindings so that previous bindings are
active during the expression form of subsequent bindings.  Of course
you can implement it as a series of nested `let`s, but its useful to
write an implementation using only `lambda`.


    (defun empty? (x) (not x)) ; sugar to test if a list is empty.
                               ; nil is the empty list, (not nil) -> t
    (defmacro my-let* (bindings &rest body)
      (cond
        ((empty? bindings) `(progn ,@body))
        (t 
         `(funcall (lambda (,(car (car bindings)))
              (my-let* ,(cdr bindings) ,@body))
            ,(cadr (car bindings))))))

Don't get spooked by this recursive macro.  All it means is that

    (my-let* ((x 10)
              (y (+ x 11)))
       (+ x y))

expands to

    (funcall 
          (lambda (x)
            (funcall (lambda (y) (progn (+ x y))) (+ x 11))
         10))

That is, each subsequent bind value expression is evaluated in the
context of a function where the previous binding symbols are already
bound. 

Holy balls!  This is almost function composition!  The `lambda`
containing our body (the `progn` form) is `f1` and each containing
`lambda` represents a composition onto this inner function.  The only
difference is that we're threading function _application_ through the
composition in such a way as to provide named values into more deeply
nested contexts.  Things are about to get serious!

Also, we are just throwing the word bind around like crazy!  Does this
use of the word bind have something to do with the `list-bind` function?

You bet it does!  You know, ordinarily I could take or leave Lisp-2's.
It seems kind of crufty to me to have to worry about a second
namespace, to have to remember to type (funcall f-var a b c) or
(funcall #'f a b c) but in this one case, its actually really useful
that we have to write out funcall explicitely.  Its useful because it
reminds us something is going on there.  What would happen if we
replaced those `funcall`s in the above expansion of `let*` with some
other function?  Let's kick this can around a bit, and see if
something falls out, shall we?

    (defun regular-bind (v f)
      (funcall f v))

So in the `let*` expansion funcall is only ever taking functions with
one argument, so the first thing we do is declare a new function which
applies a single argument function to a single value.  Why did we name
it `regular-bind`?  Well, think about the expression:

    (regular-bind 10 (lambda (x) (+ x 1))); -> 11

In `(lambda (x) (+ x 1))` `x` is _unbound_.  That is what a lambda is,
essentially, a chunk of code in which the arguments are unbound,
waiting to be _bound_ so the body can be evaluated.  `Regular-bind` is
just a function which _binds_ a value (10, above) to a variable and
executes the code.  Note: `funcall` takes `f` and then `v`, but
`regular-bind` takes `v` and then `f`.  This might make more sense
depending on how you read code ("bind v in f") or it might not.  It is
just convention.  It is called `regular-bind` because it doesn't
really do anythign that funcall doesn't already do.  Monads, though,
are all about _irregular_ bind operations.

Let's rewrite `let*` so that it uses bind instead of funcall:

    (defmacro my-let* (bindings &rest body)
      (cond
        ((empty? bindings) `(progn ,@body))
        (t 
         `(regular-bind
            ,(cadr (car bindings))
            (lambda (,(car (car bindings)))
                          (my-let* ,(cdr bindings) ,@body))))))
   
Easy enough.  

`let*` is how you thread together computations in the "identity
monad".  You may have heard someone say something like "regular lisp
lives in the identity monad."  This is what they mean.  Normally, all
bind does is associate a value with a symbol and go.  The above `let*`
expression now expands to:

    (funcall 
          (lambda (x)
            (regular-bind  (+ x 11) (lambda (y) (progn (+ x y)))))
         10)

You should be chomping at the bit now, because we've basically
invented do notation.  The question you should be asking is "What
happens when we replace bind with some other function?"  Lets make a
slightly newer macro to play with that idea.

    (defmacro monadic-let*-inner (bind-symbol binders &rest body)
       (cond 
         ((empty? binders) `(progn ,@body))
         (t 
           (let ((symbol (car (car binders)))
                 (bind-expression (cadr (car binders)))
                 (leftover-bindings (cdr binders)))
          `(funcall ,bind-symbol 
              ,bind-expression
               (lambda (,symbol)
                  (lexical-let ((,symbol ,symbol)) ; create a lexical 
                                                   ; over ,symbol.  
                                                   ; we want one
                                                   ; for lots of monads.
                   (monadic-let*-inner ,bind-symbol ,leftover-bindings ,@body))))))))

    (defmacro monadic-let* (bind-f-expression binders &rest body)
      (let ((bind-symbol (gensym "temporary-bind-symbol-")))
        `(let ((,bind-symbol ,bind-f-expression))
              (monadic-let*-inner ,bind-symbol ,binders ,@body))))

This new form takes an expression which evaluates to a bind function,
a set of let-like binders, and a list of body forms and then it just
builds the same code as a `let*` except that it uses the specified
`bind` operation rather than a mere funcall.  `Let*` sequences
computations.  `Monadic-let*` sequences operations _through a monad,_
the behavior of which is specified in `bind`.  

Let's try this crazy thing out:

    (monadic-let* 
           #'list-bind 
           ((x '(1 2 3)) 
            (y (list (+ x 1) (- x 1))))
         (list y)) ; -> (2 0 3 1 4 2)

Congratulations, friends, we are now living in the list monad.  

Let's think about what this is doing one more time.  We have a series
of expressions (the right hand portion of each binding form).  The
monadic bind operation assumes each expression is a monadic value.  In
our case, these values are lists of lisp data.  Rather than bind the
result of this expression to the symbol on its left directly, our bind
specifies that we should bind the symbol to each value inside the
monad in turn, evaluate the subsequent binding expressions in the same
way, evaluate the body, which must be a monadic value, collect each of
those values, and then, finally, combine them back into a monadic
value again before returning the result.

### That is really complicated ###
#### Why would anyone do this? ####

The reasons for doing something like this are as varied as the kinds
of monads that are out there.  In Haskell, monads are a way of writing
declarative style code in a pure fashion, but even in non-static
languages, monads can pull some neat tricks.  The sequence monad,
combined with `monadic-let*` is essentially a list comprehension.
Suppose we want to collect all the combinations of two numbers less
than 100 whose sum is less than 10.

    (monadic-let*
     #'list-bind 
     ((x (range 1 101))
      (y (range 1 101)))
      (if (< (+ x y)  10) (list (vector x y)) nil))

    ;-> ((1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) 
           (2 1) (2 2) (2 3) (2 4) ...)

Here we simply return the empty list when we don't want to put
anything into the monad because appending the empty list onto
something does not change it.  Other applications in an impoverished
language like emacs is to use the continuation monad to fake
co-routines or to use a stream monad to make dealing with lazy lists
easier.  We'll revisit this in another post.

### Notes about the monads.el library ###

I think this monad tutorial is relatively unusual in that it doesn't
spend a great deal of time belaboring the existence of the `return`
operation.  It is mentioned and used above.  A monad is technically
completely defined by specifying just two functions: `bind` and
`return`.  Bind we discussed heavily, but `return` was just thrown out
there.  Bind is its own beast, but `return` is a monadic function.  You
should know that that means `return` takes an object that isn't in the
monad and returns a monadic value.  `Return` is special in that it does
this in the simplest way possible for the monad.  For the list monad,
recall that  `return` was just `list`.  

It turns out its handy in the context of monadic operations to have
`bind` and `return` (and some other things) dynamically defined, so in
the `monads.el` library, `monadic-let*` is slightly different.  In
that library, monads are defined as hash tables containing `:m-bind` and
`:m-return` keys which associate with the appropriate functions.  Eg:

    (defvar monad-seq 
      (tbl! 
       :m-zero (list)
       :m-return (lambda (x) (list x))
       :m-bind (lambda (v f) (apply #'append (mapcar f v))))
     "The list/sequence monad.  
      Combine computations over multiple possibilities.")

Note here that `tbl!` is is just sugar for `make-hash-table`.  The
nearest equivalent to what we've called `monadic-let*` is `mlet*_`
which takes as its first argument a monad represented as a hash table.
In addition to the bind-chaining described above, it introduces a
dynamic context wherein the functions `m-bind` and `m-return` are
defined for that monad.  Because we're in emacs, and everything just
sits in one giant rotting pool of symbols, we prefix the monadic
functions with `m-` to avoid future name collisions.  The `_` at the
end distinguishes `mlet*_` from `mlet*`.  The only difference between
these forms is that `mlet*` wraps its `body` in an implicit `m-return`
so the result of an `mlet*` form is always a monadic value.  Sometimes
you want this, sometimes you don't.  So the example above would be
written in the following way using `monads.el`.
	
    (mlet*_
     monad-seq
     ((x (range 1 101))
      (y (range 1 101)))
     (if (< (+ x y) 10) (m-return (list x y)) nil))

Note that we can use `m-return` inside `mlet*`-like forms, because
they define them automatically.
    
    
`Monads.el` defines several common monads in addition to the
 sequence monad.  These include the state monad, the continuation
 monad, the set monad (parameterized on an equality predicate), an
 Error monad, and a Maybe monad.  `Monad-parse.el` defines a monadic
 parser combinator library which is really useful (to me, anyway).
    
    
### Conclusions ###
    
For me, monads didn't hit home until I walked through the process of
creating a new monad from scratch based on my own ideas.  This is
described in `weighted-graph-monad.el`, but in order to move forward
in our discussion of Kanren, the logic language, we'll need to develop
a stream monad, which facilitates work with lazy lists.  That will be
the next example.
    	

* * *




Footnote 1: Here is a bit of a confusing thing.  The monadic values of
the list monad are lists of lisp data types.  So a list of lists is,
in fact, a monadic value, which means that `r2s` above could be
considered, itself, to be a monadic value.  If this didn't occur to
you, just skip the rest of this note.  I bring it up only because if
it has occured to you, it might be somewhat confusing.  You might ask:
why don't we just return `r2s` instead of `(reduce #'append r2s)`?
This kind of of conceptual confusion is related to the fact that Lisp
is not statically typed (or is [statically typed but you are
restricted to a single type of which the various "types" of values
constitute classes][type-blog-post]). In a statically typed language
the sequence monad would be parameterized on the type of its contents,
which we'd specify where we were using it.  So we'd have a type like
`SeqM Int` (read that as "the sequence of ints monad") which would
tell use that our monadic values were lists of integers _only_, and
consequently `r2s` above would be manifestly _not_ a monadic value,
and it would be more obvious that we needed to append its contents
together before recapturing a monadic value.  In a dynamic language
this isn't as obvious, but its still conceptually necessary.  The
upshot is that the fact that `r2s` is a monadic value is a
_coincidence_, the essence of the list monad is still to _combine_ the
results of monadic functions, rather than to merely _collect_ them,
which is what returning `r2s` itself would represent. If you _did_
read this despite my warning, you're probably really confused.  Its
ok.  This stuff is confusing at first.)

[type-blog-post]: http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/) "Inflammatory Writ"
[trs]: http://tinyurl.com/2hl5bu                                           "The Reasoned Schemer"
[cc-monads]: http://richhickey.github.com/clojure-contrib/monads-api.html  "Clojure Contrib Monad Library"
[cljm-tut]: http://intensivesystems.net/writings.html                      "Clojure Monad Tutorials"
[pryor-notes]: http://lambda.jimpryor.net/                                 "Jim Pryor Notes"