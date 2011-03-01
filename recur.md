recur-defun and recur-let
-------------------------

When I first encountered clojure's limited tail call optimization, in
the form of "recur" forms inside functions and loops, I was a bit
disappointed.  Even though (obviously) I write most of my lisp code in
hoary old emacs lisp, I think I lean towards the scheme side of things
in a lot of respects, and I like writing functions recursively when
possible.  [Lots of people
disagree](http://dorophone.blogspot.com/2009/04/python-tail-call-hullabu.html),
but I find this is a clear, concise and error avoiding way of
specifying lots of algorithms.  I find loops, on the other hand, error
prone and ugly for many kinds of tasks, although the Common Lisp Loop
Macro is pretty nice for lots of simple cases.

However, if you read the above blog post, you'll notice some nuance to
my opinion which eventually made me warm to the `recur` form in
clojure.  Since the stack is a limited resource in most programming
systems for reasons of optimization, and since tail calls don't grow
the stack, and are therefore something like "meta-semantically"
different than non-tail calls, I rather like that Clojure forces you
to explicitly mark an attempted tail call, and barfs at you if you
place it in a non-tail location.  This seems like a good idea, even if
you lose a bit of elegance in not using the name of the function
itself to recur.  Reflection on this theme eventually reveals that the
real limitation of `recur` in Clojure is that it only allows
self-recursion.  A conforming scheme optimizes all tail calls,
regardless of their location.  In practice, lots of algorithms depend
on self-recursion only (but see the Ackerman numbers for a standard
counter example of a sort - they are usually computed with several
mutually recursive functions), and since the JVM doesn't support
complete tail call optimization, `recur` represents an elegant
compromise between slow, trampoline based full optimization, and
relying entirely on loops or other ad-hoc iteration techniques.

I promise I am getting somewhere.

[Once
again](https://github.com/VincentToups/emacs-utils/blob/master/multi-methods.md)
I am faced with a comparison between Clojure and Emacs Lisp, because
both are Lisps which find themselves living in somewhat primitive
run-times.  Emacs Lisp is somewhat constrained by legacy
considerations (although I don't rightly know why the EL engine
doesn't optimize tail calls - if anyone does, please contact me) and
frankly, somewhat outmoded design.  Clojure, similarly saddled with
the JVM, shows considerable insight and care in its design, providing
modern, powerful features to a difficult environment.  `Recur` is one
such feature.

I wanted to add this capability to Emacs Lisp because I kept running
into places in other projects where I was writing big, gross loops
where a recursive implementation would have been easier to write and
read.  Technically, I've already done this since I've ported all of
Clojure's `defn` and `loop\let` destructuring bind forms into Elisp,
and these forms provide for the use of the `recur` keyword without
growing the stack.  Read about those features
[here](https://github.com/VincentToups/emacs-utils/blob/master/README.md).
However, the "Clojure-like emacs" project was one of my first very
large forays into Emacs Lisp and Programming Language Implementation,
and even though I frequently use it, it is a little slow (unless one
byte-compiles it) and frightening to use.  The implementation is
crufty and needs an overhaul.

So I decided I'd re-implement the features I wanted in a more Emacs
Lisp flavored way.  This library provides those features in two forms.
One is a `recur-let` form which allows recursive calls on a let's
binding forms, similar to Scheme's `named-let` feature.  The other is
a `recur-defun*` form, which allows you to declare a function (with a
common lisp-like lambda-list) which can `recur` to itself using the
recur keyword.  Neither forms grow the stack during their
pseudo-recursion.  It is quite possible to write infinite loops, for
instance.

Examples Are In Order
---------------------

Here is an example of `recur-let`
    
    (require 'recur)
    (recur-let ((x 0))
      (if (< x 10) (recur (+ x 1)) x))

This evaluates to 10.

Here is an example of `recur-defun*`

    (recur-defun* my-prod (list &optional (acc 1))
      (if list (recur (cdr list) (* acc (car list)))
          acc))
    (my-prod (list 1 2 3 4)) 

The second form evaluates, as it should, to 24.  (Thanks to Joseph Gay
for the correction).

Conclusions
-----------

This implementation is much, much simpler than the implementation
associated with `defn` and its kin from my clojure library.  Although
this is a very early version, I think the library is stable enough to
use for real things.

Most of the complexity here was not even related to the implementation
of recursion itself, but with parsing lambda-lists, for which emacs
seems to have no native functions.  I am sure I could have used some
piece of `cl.el` code, but a cursory search didn't reveal anything.

Most of the code is commented.  I hope someone out there finds this
useful.  If you are interested in code-walking macros, this might be a
good example to get a sense for how they work and what they do.  The
critical part of this implementation is a codewalker which searches
for `recur` and expands it appropriately or throws an error if we
aren't in tail position.

Thanks for reading!





  

