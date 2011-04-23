Deep Elisp Part 1
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
about implementing Kanren itself, but I've never written up a piece of
documentation on the implementation of Monads I've cooked up for
Elisp, so I thought I'd start there.

So, this document one part monad tutorial and one part introduction to
the `monads.el` implementation.  

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

We've reached the monad tutorial part of this document.  Like most
people, I found monads pretty hard to understand when I first
encountered them.  Most monad tutorials speak in Haskell, which I
think can be doubly confusing because its hard to tell exactly where
Haskell ends and where monads begin, since Haskell has special syntax
built in to the language just for the purpose of dealing with monads.
You can, of course, use the functions `>>=` and `return` outside of do
notation, as many tutorials do, but at least dealing with `>>=`
requires a fair amount of comfort with Haskell's infix operator
system.  At least it seemed that way to me. 

I really had to implement monads in Elisp to finally get it.  I think
this worked because every part of the monad beastiary had to be built
from the ground up.  If you want  "do notation," for instance, it
isn't just there, you have to write a macro for it, and understand
it.  The uniformity of Lisp syntax (which some people hate, I know)
also helps keep things focused on the ideas.  That is the approach we
will take here.  We'll start just kicking around some general ideas
and gradually build up to `do` notation, making constant analogy to
well understood lisp and programming concepts.  I think this is better
than throwing oneself in head first. 

### Notes on Types ###

I think of a monad as a way of decorating function composition.  If I
have a group of functions that conform to a particular input and
output constraint, a monad associated with that constraint tells me
how to combine those functions so that something interesting happens.
The fact that these functions all have the same type of input and
output is critical, otherwise the "plumbing" specified by the monad
won't work.  Such functions are called "monadic functions" and when we
try to understand monads, or a specific monad, we must meditate on the
type of these functions.

When coming to monads from dynamic languages, it can be easy to gloss
over all the type language used to describe them.  But it really helps
to think about types in a loosy-goosey way, at the very least, because
type constraints are what make the plumbing in a monad work.  Monads
generally contain things.  In statically typed languages the monad
type depends on the type of the things it holds, so it would be
specified, but we'll just refer to these things as "values".  Their
type isn't terribly important except that they are distinct from the
type of *monadic values*. 

A *monadic value* can be thought of as an abstract container which
contains regular values.  Sometimes this is a rather obvious kind
of relationship.  A list "contains" values, for instance.  Sometimes
it can be much more abstract.

Finally, there are *monadic functions*.  A monadic function is a
function which takes a regular value and returns a *monadic value*.
It is important to think about these types while we work on the rest
of the tutorial, so take a second to look at the diagrams, and then
get into the habit of calling to mind the types of various objects
whenever you get confused.  It really helped me.

![Types of Interest](http://dl.dropbox.com/u/1076954/monadic-types-of-interest.png "Types of Interest")

### The List or Sequence Monad ###

We'll be talking about the sequence monad first.  We will represent
sequences with regular old lisp lists, which can hold values of any
lisp type.  The monadic values, then, are lists.  Lists "contain"
(they also contain) lisp data.  Let us meditate upon the type of
monadic functions in the sequence monad.

==== this space reserved for meditation ==== 

If you guessed in your meditation that the monadic functions
associated with the sequence or list monad (we'll use the two
interchangeably from here on) are functions which take a single value,
which is any lisp data, and return a list of lisp-values, you got it.

So, whatever the hell a "list monad" is, its got something to do with
composing functions which take a single lisp value and return a list.  

What kind of functions might do this?  Well, a common idea is that
functions in the list monad represent computations with multiple
possible outcomes or values.  A person might have multiple friends, so
a function `get-friends` might return a *list* of persons rather than
a single person.  We might want to go on to calculate those persons'
friends (or enemies, relatives, lovers).  All these `get-` functions
would be monadic functions in the list monad, and the list monad will
help use deal with them.

Let's start kicking around ideas about composing these functions.  To
do that we need a few functions to consider explicitly.  What are the
simplest monadic functions of the list monad?  Well `list` is
certainly such a function when you call it with one argument, so lets
put that on the pile.  In that spirit, consider:

    (defun stub (x) nil)
    (defun list-return (x) (list x))

![monadic return](http://dl.dropbox.com/u/1076954/monadic-return.png "monadic return")

`list-return` can be thought of as a sort of "default" or "simplest
possible" monadic function.  We've just wrapped up `list` so that it
takes but one argument.  Every monad needs a return, and every return
does something like the above - it wraps its input into the monad in a
simple way.

`stub` is also a monadic function - it ignores its inputs and returns
the empty list.  

Ok, lets write some semi-meaningful list-monadic functions.  Consider:

    (defvar *people* '(:ted :lea :leo :james 
                       :harvey :sally :jane :andrew 
                       :catherine) 
                       "A list of all the people that matter.")
    (defvar *friends-db* 
          '((:ted (:lea :leo :sally :andrew :catherine :leo :jane))
            (:lea (:ted :leo :jane :andrew :harvey :sally :catherine))
            (:leo (:ted :lea :ted :harvey :sally :jane :andrew 
                        :catherine :harvey :andrew :catherine))
            (:james (:jane :harvey :jane))
            (:harvey (:leo :lea :leo :james :harvey :harvey :sally))
            (:sally (:ted :leo :lea :harvey :jane :andrew))
            (:jane (:lea :leo :james :sally :ted :james :andrew :catherine))
            (:andrew (:ted :lea :leo :sally :jane :leo))
            (:catherine (:ted :leo :lea :leo :jane :catherine :catherin))) 
    "Our database of friend connections.")

    (defun friends-of (person) 
      "Return a list of all the people in friends-db."
      (alist *friends-db* person)
             ;alist is a function which retrieves
             ;a key's data from an association list.
    )

    (defun mutual-friends-with (person1)
      "Return a monadic function which returns 
       the friends person1 has in common with person2."
      (lexical-let ((p1-friends (friends-of person1)))
        (lambda (person2) 
          (let ((p2-friends (friends-of person2)))
           (filter (lambda (friend)
                     (in friend p1-friends)) p2-friends)))))

`Friends-of` is more or less obvious enough.  It simply retrieves
the database entry in the friends database, here represented as an
association list.  It takes a person and returns a list of persons, so
it is a monadic function.  

The second function is a little more complicated.  It takes a person
and returns a function, so its not a monadic function itself.  It
does, however, return a monadic function - one parameterized on
`person1`.  It returns the friends of `person2` that are also friends
with `person1`.  This business of having a function which returns a
_parameterized_ monadic function is pretty common, and it will lend
monadic expressions a certain domain-specific-language feel, as you'll
see.  Note that we need a `lexical-let` in the function body to create
a closure around `p1-friends`.  If we used a `let`, `p1-friends` would
be out of scope by the time someone called the `lambda`.  If this is
confusing, see the wikipedia article on [scope in programming
languages][scope].  Emacs has a default dynamic scope, but most modern
languages are lexical<sup>2</sup>.   

But we were meditating on fun!tion composition.  Before going any
further lets implement a function which composes functions.  The only
wrinkle here is that order of composition is reversed from the order
of application (`f1` is applies before `f2`) because this looks more
natural in an applicative language.

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

Let's try just composing some of our list monadic functions.  We might
actually want a function to calculate the `friends-of` the
`friends-of` someone:

    (compose #'friends-of #'friends-of)

Because this is Elisp, this doesn't cause an error.  Of course, if we
think about this, we know it can't work.  As soon as well call this
function, we'll get an error, because the second friends-of will be
applied to a list, rather than a single person.  Try it, if you are
having trouble seeing it.

Suppose we were dead set on jamming these two functions together and,
in particular, we want to make sure that the resulting function can be
used wherever either of the two functions might have been used.  That
is, we want the result type to be the same as the input types.  There
are several approaches, obviously, but here is a sort of obvious one
(read the comments here, they are important):

    (defun list-func-compose (f2 f1)
       (lexical-let ((f1 f1)
                     (f2 f2))
        (lambda (arg) ; lambda need take only a single arg, as f1 must
          (let* ((r1 (funcall f1 arg)) 
            ; r1 is a list, because it is the result of f1, a monadic function.
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

Pause to consider what happened here.  We apply `f1` to the input,
which produces a list (a _monadic value_).  `F2` takes values, not
monadic values, so we just `mapcar` `f2` over the values in the list.
`F2`, like `f1` returns a list, so now we have a list of lists, which
we concatenate into a big list, which is now our output monadic value.

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
           (let ((monadic-value (funcall f1 value)))
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

Another way of thinking of it is that if `list` describes how to turn
a value into a monadic value, `list-bind` relates a mondadic function
to a function which both operates on and returns monadic values.

![Monadic Bind](http://dl.dropbox.com/u/1076954/monadic-bind.png "Monadic Bind")

Ok, believe it or not, we've basically covered the entire sequence
monad already.  All that is left is to really get the idea and to
understand how it relates to Haskell's do notation and the canonical
monad operations.  But it bears repeating - "do notation" isn't the
same thing as monads.  It is just syntactic sugar.  Its might be best
to understand monads before do notation, depending on your
temperament. 

Let's try things out, however.  

    (funcall (list-func-compose #'friends-of #'friends-of) :leo) 
    (:lea
     :leo :sally :andrew :catherine :leo :jane :ted :leo :jane :andrew
     :harvey :sally :catherine :lea :leo :sally :andrew :catherine :leo
     :jane :leo :lea :leo :james :harvey :harvey :sally :ted :leo :lea
     :harvey :jane :andrew :lea :leo :james :sally :ted :james :andrew
     :catherine :ted :lea :leo :sally :jane :leo :ted :leo :lea :leo
     :jane :catherine :catherin :leo :lea :leo :james :harvey :harvey
     :sally :ted :lea :leo :sally :jane :leo :ted :leo :lea :leo :jane
     :catherine :catherin) 

(We're probably really interested in the unique values of this list.
We could use something called the set monad to get those, or we could 
just pass this result through `unique`.

Pretty anti-climactic, for sure.  But we can do more interesting
things:

    (funcall 
      (list-func-compose 
         (mutual-friends-with :lea) #'friends-of)
      :leo)

This composition gives a list of the friends of Leo who are also
friends with Lea.  As indicated above, what we've done here is
inserted a parameterized monadic function into the composition.  This
turns out to be useful.  It also hints at the next step.  What if we
wanted to parameterize a monadic function based on some of the values
"coming down the pipe" in the monad?  How could we do that?  In a
regular function composition all those values are invisible.  Is there
way we can name them which preserves the utility of this approach?

### Stepping Back ###

Monads in one sentence: a monad is set of operations which relate
specific functions called _monadic functions_ and specific values,
which are either naked _values_ (the input type to _monadic
functions_) or _monadic values_, which is the output type of monadic
functions.  

In particular, the _bind_ operation knows how to pull values out of
monadic values, apply monadic functions to them, and collect all the
resulting monadic values into one big monadic value.  Using _bind_ we
can compose or otherwise manipulate monadic functions in a controlled
way.

Everything else is window dressing.

### Window Dressing ###

Let's talk about `let*`.  We've been twisting up our brains into knots
over monads, so let's let those knots untwist and return to this simple,
clean, construct.  In Lisp, variable bindings are explicitly
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
that we have to write out funcall explicitly.  Its useful because it
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
really do anything that funcall doesn't already do.  Monads, though,
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
happens when we replace bind with some other function?"  Let's make a
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
                   (monadic-let*-inner 
                       ,bind-symbol ,leftover-bindings ,@body))))))))

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
declarative style code in a pure fashion, but even in non-static,
non-pure languages, monads can pull some neat tricks.  The sequence
monad, combined with `monadic-let*` is essentially a list
comprehension.  Suppose we want to collect all the combinations of two
numbers less than 100 whose sum is less than 10.

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
nearest equivalent to what we've called `monadic-let*` is `mlet` (the
star is removed because monadic binding mostly makes sense as a
sequential thing) which takes as its first argument a monad
represented as a hash table.  In addition to the bind-chaining
described above, it introduces a dynamic context wherein the functions
`m-bind` and `m-return` are defined for that monad.  Because we're in
emacs, and everything just sits in one giant rotting pool of symbols,
we prefix the monadic functions with `m-` to avoid future name
collisions.  So the example above would be written in the following
way using `monads.el`.
	
    (mlet
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
    
### Sujar ###

The monad library can make use of my implementation of Clojure-style
destructuring bind too.  One can write the above expression as

    (domonad monad-seq 
     [x (range 1 101)
      y (range 1 101)]
     (if (< (+ x y) 10) (m-return (list x y)) nil))

Besides the superficial change of using a vector for the binding
expressions, you can use arbitrary nested binding forms inside.  If
you are familiar with clojure, it works basically like that, except
that tables are destructured with `[:: ]` instead of `{ }` and you can
use table-destructuring on hash tables and alists.  
    
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
constitute classes][type-blog-post]). 

In a statically typed language the sequence monad would be
parameterized on the type of its contents, which we'd specify where we
were using it.  So we'd have a type like `SeqM Int` (read that as "the
sequence of ints monad") which would tell use that our monadic values
were lists of integers _only_, and consequently `r2s` above would be
manifestly _not_ a monadic value, and it would be more obvious that we
needed to append its contents together before recapturing a monadic
value.  In a dynamic language this isn't as obvious, but its still
conceptually necessary.  The upshot is that the fact that `r2s` is a
monadic value is a _coincidence_, the essence of the list monad is
still to _combine_ the results of monadic functions, rather than to
merely _collect_ them, which is what returning `r2s` itself would
represent. If you _did_ read this despite my warning, you're probably
really confused.  Its ok.  This stuff is confusing at first.)

* * *

Footnote 2: This is a long, diversionary footnote.  Probably its best
to finish the rest of the document before reading.

For some reason I find it useful/pleasurable/amusing to consider the
question of monads in languages without scope at all.  Namely, stack
languages like [Factor][] or [Joy][].  Not only are these languages
without scope, by default, variables are not even named.  Values are
merely pushed onto and pulled off of a stack which is passed
implicitly between "words," the analog of functions:
  
    (require 'with-stack)
    (||| 4 4 + 3 -) ; -> 5

Here we are using a stack language interpreter which lives inside
emacs lisp.  You can get "with-stack.el" from my github.

Without a means of binding variables, "lambdas" become merely
"quotations," simple lists of literals and words which can be executed
by words like `call`.

    (||| 4 '(4 +) call '(3 -) call) ; -> 5

In the "emacs stack language" quotation serves directly as the quote
operator, and quotations are represented as lists.  Executing a
quotation is the same looping over it, executing each word as it is
encountered, or pushing atoms onto the stack.  

The nakedness of these quotations encourages languages to use them as
blocks in control structures:

    (||| 1.0 1>random* .5 > '("true" print) '("false" print) if) 

The above will print "true" 50% of the time and "false" 50% of the
time.  I don't want to get bogged down with a full `with-stack`
tutorial, but "1.0 1>random*" means "push 1.0 onto the stack and call
the emacs lisp function `random*` with 1 argument from the stack,
pushing the result".  The word `if` takes a boolean and two
quotations, executing the first when the bool is true and the second
when it is false.  

Ok, so quotations are lambdas.  What are monadic quotations of the
list monad?  These must be quotations which take a value off the stack
and return a list of values.  The quotation version of `return` for the
list monad would be:

    (||| word: list-return 1>list end:)

That is, take a value off the stack and return a list with that value
in it.  

What about `bind`?
    
    (require 'stack-words)
    (||| word: list-bind ;( mv f (v ;; mv) ;; mv )
         map '(append) reduce end:)

Characteristically, the word version is quite terse.  Interestingly,
we hardly need to have an analog of do notation, because variables are
not named and application is merely juxtaposition:

    (||| word: bi>list ;( item qtn1 qtn2 ;; ( res1 res2 ) )
         bi 2>list end:)
    (||| '(1 2 3) 
         '( '(1 +) '(1 -) bi>list )
         list-bind
         '( '(3 +) '(3 -) bi>list )
         list-bind) ;-> (5 -1 3 -3 6 0 4 -2 7 1 5 -1)

(The word `bi>list` takes an item and two quotations, applies the
quotations each to the item and collects the results into a list of
two elements.  It assumes the quotations take one item off and push
one onto the stack.)

Wherever we want to use monadic behavior, we simply replace `call`
with `list-bind`.  We can write a word to make this more pleasant:

    (||| word: fold-bind-into ; ( qtnseq bind-qtn ;; qtnseq )
          '( curry ) curry map 1>flatten-once end:)
    (||| word: monadically ; ( init-val qtnseq bind-qtn ;; result )
           fold-bind-into call end:)

    (||| '(1 2 3)
           {{ '( '(1 +) '(1 -) bi>list )
              '( '(3 +) '(3 -) bi>list ) }}
         '(list-bind) monadically )

Where `{{` is a parsing word which makes a list of what is on the
stack between `{{` and `}}`.  We could have used quote to make the
list of quotations, but we'd be getting a lot of quote marks on the
screen.  All this word does is fold the contents of a binding
quotation (here a simple call to `list-bind`) into the sequence of
quotations we want to operate monadically.  Then we simply call the
resulting quotation.  

Assuming some comfort with the idioms of concatenative languages, the
result is an incredibly simple demonstration of the essence of monadic
operation.  It strikes me that it is often the lowest-level words
which are the hardest to read and write in concatenative languages,
while high level code has an almost comic simplicity.  I wonder what
this means?


* * *

[type-blog-post]: http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/ "Inflammatory Writ"
[trs]: http://tinyurl.com/2hl5bu                                           "The Reasoned Schemer"
[cc-monads]: http://richhickey.github.com/clojure-contrib/monads-api.html  "Clojure Contrib Monad Library"
[cljm-tut]: http://intensivesystems.net/writings.html                      "Clojure Monad Tutorials"
[pryor-notes]: http://lambda.jimpryor.net/ "Jim Pryor Notes"
[scope]: http://en.wikipedia.org/wiki/Scope_(programming) "Scope in Programming"
[Factor]: http://factorcode.org/ "Factor Language"
[Joy]: http://www.latrobe.edu.au/philosophy/phimvt/joy/j02maf.html "Joy"

