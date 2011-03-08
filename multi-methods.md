Multimethods For Emacs Lisp
---------------------------

One of my favorite ways of understanding something is to implement/do
it.  This kind of reasoning has led to some of the other projects
here, like implementations of [Clojure's destructuring bind and
monads](https://github.com/VincentToups/emacs-utils/blob/master/README.md)
in emacs lisp.  These projects highlight the usefulness of emacs as a
computer science experimentation environment, but they are arguably
not very _practically_ useful: you can implement almost anything in a good lisp, but
that doesn't mean the easy implementation will be fast or usable, in
terms of error messages and failure behavior (although I make some
effort towards this).

Today's project, based on Clojure's Multimethod system, is somewhat
different.  The same concerns which make multimethods make sense for
clojure make it a pretty good match for emacs lisp too (at least to my
untutored eye).  Clojure lives in the Java ecosystem, and so depends
on and interacts with Java's object model.  Yet its semantically
somewhat different than Java, and it isn't itself object oriented.
Multimethods provide a flexible system on which one or more
object-oriented systems can be constructed, in parallel or enmeshed
with the underlying object system.  Similarly, emacs has no native
object system.  Multimethods provide a flexible way to introduce
systems of graded complexity into emacs code for when you want
something smaller or more integrated than [EIEIO](http://cedet.sourceforge.net/eieio.shtml), for instance.

What are Multimethods?
----------------------

Multimethods are a kind of proto-object system.  Unlike object systems
in most languages (but like CLOS), they allow dispatch on multiple
arguments.  A simple example of this is generic rendering code.  If
you want to create a game which renders both to a graphical window and
a text console, depending on the mode, you might want a method draw
which looks at both the object to be drawn (one of the many objects in
a game) and the target, either a text buffer or an sdl-surface or
something similar.  

We need just a bit of front matter before proceeding.  This library
depends on a big pile of utilities, one of which is a set of
association-list manipulation functions.

    (setq example (alist>> :x 10 :y 11)) ;-> ((:x 10) (:y 11))
    (alist! example :y 14) ;-> ((:x 10) (:y 14))
    example ;-> ((:x 10) (:y 14))
    (alist>> example :y 21) ;-> ((:x 10) (:y 21))
    example ;-> ((:x 10) (:y 14))
    (alist example :x) ;-> 10

In other words, `alist>>` creats and functionally updates an alist.
`alist` extracts values from one.  `!alist` destructively updates an
alist.  We'll be using alists as ad-hoc class structures in the rest
of this document, but multimethods can dispatch on any kind of
variable using any sort of function.  Bear that in mind.  We could
just as easily be using hash tables.

With the above in mind, lets look at the example.  We'll use alists to
represent classes.  Here is how we declare a multi-method.

    (require 'multi-methods)
    (defmulti draw (over-all-args :class) "Generic drawing method.")

This declares that `draw` is a generic method which uses :class,
dispatched over all arguments, to draw things.  `over-all-args` is a
function which takes a function or a keyword and, if a function,
returns a new function which maps the original over all the new
functions arguments.  If the input is a keyword, then it assumes all
args are tables, and pulls out that element of the table:

    (funcall (over-all-args :class) 
      (alist>> :class :c1)
      (alist>> :class :c2)) ;-> [:c1 :c2]

(It returns a vector, for reasons of syntactic convenience).

Now lets declare some specific implementations of `draw`.

    (defmulti draw (over-all-args :class) "Generic drawing method.")
    (defunmethod draw [:player :text-console] (player screen) 
      "Here would be code to draw an @ sign to represent the player")
    (defunmethod draw [:monster :text-console] (monster screen)
      "Here would be code to draw an M sign to represent a monster.")
    (defunmethod draw [:player :sdl-surface] (player surface)
      "Here would be code to find the sprite representing a player and
          blit it onto the sdl surface")
    (defunmethod draw [:monster :sdl-surface] (monster surface)
      "Here would be code to find the sprite representing a monster and
          blit it onto the sdl surface")

And some elements to work with.  These are obviously stubs: we've just filled in the
class and nothing else.  But we could populate these alists with other information
(and we'd need to).    
    
    (setq player (alist>> :class :player))
    (setq surface (alist>> :class :sdl-surface))
    (setq monster (alist>> :class :monster))
    (setq text-screen (alist>> :class :text-console))
    (setq griffon (alist>> :class :griffon))

Now lets make it clear that a griffon is a subclass of a monster.
	
    (derive :monster :griffon)

And now test some calls:

    (draw player surface) ;-> "Here would be code to draw an @ sign to represent the player"
    (draw griffon text-screen) ;-> "Here would be code to draw an M sign to represent a monster."
	
Note that drawing a griffon invokes code to draw a monster, because `:griffon isa? :monster`.  If we want specific behavior for the griffon, we can define specific methods:  

    (defunmethod draw [:griffon :text-console] (g s)
      "draw a griffon specifically, to a text screen")

    (defunmethod draw [:griffon :sdl-surface] (g s)
      "draw a griffon specifically, to an sdl screen")

    (draw griffon surface) ;-> "draw a griffon specifically, to an sdl
    screen"

Super
-----

A frequent idiom in object oriented programming is to call a method's
`super`, that is, its implementation corresponding to the object from
which the current object is derived.  We have a multiple dispatch
system here, so there isn't a simple notion of super, however you
might still want to accomplish the same thing.  You do that using one
of a trio of functions: `get-method`, `get-method-funcall` or
`get-method-apply`.  Each takes as its first two arguments a method
name (a symbol), and a dispatch value.  `get-method` simply returns a
the method, if it exists, corresponding as closely as possible to the
dispatch value.  `get-method-funcall` calls that same method with the
arguments passed in, and `get-method-apply` does the same thing,
except it applies the method to the list passed in.  If we wanted
to use this in the case of the griffon, we'd say something like:

    (defunmethod draw [:griffon :sdl-surface] (g s)
      (get-method-funcall 'draw [:monster :sdl-surface] g s)
      <griffon specific code>)

Caveats, etc
------------

This library uses code to resolve method dispatch which tries to find
the most specific combination of `isa?` relationships possible.  This
means that method dispatch will always choose the the match closest to
the dispatch value from the dispatch table.  This can result in
ambiguities.  In that case, you must specify the resolution
explicitely with `prefer-method,` which takes a method name and two
dispatch values which presumably conflict.  The first is marked as
preferred.

Right now the dispatch algorithm is my own custom job (I didn't look
at the clojure source code).  It might have some edge cases I haven't
sanded off.

Conclusion
----------

I think multimethods are probably useful and well suited to emacs
lisp.  They are lightweight, powerful and coexist well with the emacs
ecosystem.

Notes/Updates:
--------------

I added method dispatch caching, so method look up should be constant
time now, after dispatch has been calculated a few times.  I also
added a bunch of forms to underive and undefmethod so that you can
easily develop systems without nuking your whole hierarchy or method
system.  