Deep Emacs Lisp Pt 2
====================
Streams
-------

Last time we went through a lengthy development of monads in emacs
lisp.  Most of our attention was focused on just getting the idea
down, and we developed a pretty full list monad.  

It was intimated at the time that the list monad can be thought of as
a "possibilities" monad.  That is, we have functions which depend on a
single input, but can produce many possible outcomes.  

Suppose we wish to understand the probabilty of of rolling a given
number when combining N dice with different numbers of sides.
Characterizing a particular fair die is easy enough.

    (require 'utils)
    (defun die-outcomes (n-sides) 
      (range 1 (+ n-sides 1)))
    (die-outcomes 6) ;-> (1 2 3 4 5 6)

(Note: All the code here depends on my `utils.el` and other packages
available from [my github page][emacs-utils]).  

You might want to know: what is the probability of rolling a 10 when I
roll three six sided dice?  You might have noticed that `die-outcomes`
is a monadic function in the list monad.  It takes a number and
returns a list of outcomes.  How can we use the monad to calculate the
probability in question?

    (require 'monads)
    (let* ((outcomes (mlet*_ monad-seq
                      ((d1 (die-outcomes 6))
                       (d2 (die-outcomes 6))
                       (d3 (die-outcomes 6)))
                      (m-return (+ d1 d2 d3))))
           (n-outcomes (length outcomes))
           (n-tens (length (filter (par #'= 10) outcomes) )))
       (* 100 (/ n-tens (* 1.0 n-outcomes)))); ->  12.5 percent chance

This is a kind of interesting thing, if you think about it.  While
this certainly represents a brute force approach, the really neat
thing is that we only just described the possible outcomes of each
die.  This is a very easy problem.  We then just let the list monad
sort out the details.  

I just joined the Triangle Area Functional Programmers Group, and they
recently considered the "Cracker Barrel Peg Board Puzzle" question.
If you aren't familiar with the game, here is a guy solving it with a
mnemonic device:

<iframe title="YouTube video player" width="480" height="390"
src="http://www.youtube.com/embed/QMHGesvEyXQ" frameborder="0"
allowfullscreen></iframe>

But mnemonic devices are pretty crappy programs for pretty crappy
computers (brains).  Can we do better?  Well, in the spirit of the
above dice example, instead of considering the problem "how do we
solve the peg board puzzle?", let us consider instead the simpler
problem: given a peg board, how do we enumerate all the moves that we
can make on any given turn?

We're going to be working functionally, so lets decide an on a
persistent representation of a game board.  By persistent we here mean
that when we modify such a structure, we actually get a copy of that
structure back.  One such representation is :

    (defun fresh-board ()
      (alist>>
       0 '(0)
       1 '(0 1)
       2 '(0 1 2)
       3 '(0 1 2 3)
       4 '(0 1 2 3 4)))

We just represent a board as an alist with row indexes as keys and
each row represented by a list of occupied positions.  `alist>>` is a
function from my utilities which just turns the above into:

    ((0 (0)) (1 (0 1)) (2 (0 1 2)) (3 (0 1 2 3)) (4 (0 1 2 3 4)))

Adding a removing a peg are somewhat obvious.  We just dip into the
alist to the appropriate row and remove the number of the peg we want
to take out.  Adding is the opposite operation.  We will handle error
checking at another level of abstraction, but we will at least
maintain at this point that each row contains only unique peg
positions.

We'll represent positions as cons cells with column as the cdr and row
as the car.  

    (defun pos (x y)
      (cons x y))

And we'll be destructuring positions a lot, so lets whip up a quick
macro for that:

    (defmacro let-pos (peg-binders &rest body)
      (cond ((empty? peg-binders) `(progn ,@body))
            (t (let ((binder (car peg-binders))
                     (peg-sym (gensym "peg-")))
                 `(let* ((,peg-sym ,(cadr binder))
                         (,(car (car binder)) (car ,peg-sym))
                         (,(cadr (car binder)) (cdr ,peg-sym)))
                    (let-pos ,(cdr peg-binders) ,@body))))))

Example:

    (let-pos (((x y) (pos 3 4)))
      (list :x x :y y)) ;-> (:x 3 :y 4)

There are more general destructuring bind operations lurking in the
fetid depths of my emacs lisp library, but clarity demands we use this
simple solution.  Note that the macro accepts any number of position
binding forms.

Now those board-related functions:

    (defun peg-at-board? (board pos)
      (let-pos (((x y) pos))
               (mlet* monad-maybe^i
                      ((row (alist board y))
                       (at? ($ x in row)))
                      at?)))

    (defun remove-peg (board pos)
      (let-pos (((x y) pos))
               (alist-conjugate board
                                y
                                (lambda (row)
                                  (filter 
                                   (f-not (par #'= x)) row)))))

    (defun n-sort-cons (n n-list)
      (cond ((empty? n-list) (list n))
            ((= n (car n-list)) n-list)
            (($ n < (car n-list)) (cons n n-list))
            (t (cons (car n-list) (n-sort-cons n (cdr n-list))))))

    (defun add-peg (board pos)
      (let-pos (((x y) pos))
               (alist-conjugate board
                                y (pal #'n-sort-cons x))))

Don't worry to hard about these functions.  They just do what they say
on the tin, with the understanding that they return a *new* board with
the indicated changes, rather than modifying the board.  `n-sort-cons`
is not tail recursive, but since there are no more than five pegs in a
row, we aren't likely to blow the stack.

It will be handy to simulate moving pegs in a way that results in
failure when we try to move off the board.  

    (defun on-board? (pos)
      (let-pos (((x y) pos))
               (and (>= y 0)
                    (<  y 5)
                    (>= x 0)
                    (<= x y))))

    (defun move1 (pos direction)
      (let-pos (((x y) pos))
               (let* ((new-pos
                       (case direction
                         (:nw (pos (- x 1) (- y 1)))
                         (:ne (pos x (- y 1)))
                         (:e (pos (- x 1) y))
                         (:w (pos (+ x 1) y))
                         (:sw (pos x (+ y 1)))
                         (:se (pos (+ x 1) (+ y 1))))))
                 (if (on-board? new-pos)
                     new-pos
                   nil))))

`move1` takes a starting position and a direction (one of `(:nw :ne :e :w
:sw :se)`) and returns the new position, if it is on the board.  Using
`move1` we can define `move-n` which just repeats this process N
times, or until we fall off the board.  You can see this code on the
github, its straightforward.  

Ok, finally something interesting: 

    (defun generate-hop (board pos dir)
      (lexical-let ((pos pos))
        (mlet*_ monad-maybe^i 
                ((origin-occupied? (peg-at-board? board pos))
                 (over (move1 pos dir))
                 (target (move-n 2 pos dir))
                 (over-occupied? (peg-at-board? board over))
                 (target-empty? (not (peg-at-board? board target))))
                `((:remove ,pos)
                  (:remove ,over)
                  (:place ,target)))))

This function takes a board, a position, and a direction and generates
a hop in that direction if one is allowed.  It returns nil otherwise.
This kind of thing is built for the maybe monad, so most of the
function lives there.  It is a good time to remind ourselves of how
monads work.  `mlet*_` tells use we are going to be chaining our
binding through a monad, in this case `monad-maybe^i`, which is
defined in `monads.el`:

    (defvar monad-maybe^i
      (tbl!
       :m-zero nil
       :m-return (lambda (x) x)
       :m-bind (lambda (v f)
                 (if (not v) v
                   (funcall f v))))
      "The (implicit) MAYBE monad.  
       NIL indicates failure.  
      MaybeVal is the identity.  
      Just is the identity.")

This monad is a variation on the regular old `maybe` monad which just
lets the programmer express failure with a regular old lisp `nil`
instead of a tagged value.  Hence the zero of this monad is just nil.
Return is the identity function, and bind is `almost` funcall, but not
quite.  If the monadic value passed in is nil, it doesn't apply the
monadic function.  It just returns nil.  The net effect is that if any
expression in the `mlet*` expression above is nil, the whole
expression evaluates to `nil`, short circuiting through the subsequent
expressions.  Handy.  Monads are cool (see footnote 1 for why this 
monad has an `^i`).

Ok, so now we have a function which can generate a hop.  It returns a
list of instructions on how that hop is implemented which we can use
to modify a board to effect that hop.  These instructions can be
thought of as a really dead simple programming language, so we can
write a function:

    (defun interpret-hop (board hop)
      (reduce 
       (lambda (board move) 
         (let ((what (car move))
               (where (cadr move)))
          (case what
            (:remove (remove-peg board where))
            (:place  (add-peg board where)))))
       hop
       :initial-value board))

Which takes a hop and modifies the board to account for that move.
Apart from a bit of book keeping, we've solved the peg-puzzle.  We can
write a function which returns all the legal hops for a given board
quite easily using the `list-monad`.

    (defun generate-hops (board)
      (if (board-solved? board) (list board)
          (mlet*_ monad-seq^i 
            ((direction       (list :nw :ne :e :w :sw :se))
             (position        (generate-positions))
             (hop             (generate-hop 
                               board position
                               direction)))
           hop)))

(Astute readers might notice we could partially apply the board
argument of `generate-hop` and then `lift-2` the resulting function
into the sequence monad, then apply it to the lists of positions and
directions.  The provided approach is probably easier to read for people
not too familiar with monads.)

Now solving the problem is just matter of applying generate-hops
thirteen times, taking care to produce a new set of boards from the
generate hops at each step.  

If you try this, you'll find that it takes a very long time.  The peg
game has a pretty large state space, and its a hassle to generate ALL
solutions when we really don't want all of them, at least not all at
once.  Can we recapture the elegance of this simple, declarative
solution without having to calculate _every single_ win condition?

Enter Streams
-------------

One way we can use essentially the same methodology but not have to
calculate all the answers is to use _lazy_ lists or _streams_.  Emacs
doesn't have them, so we are going to have to roll our own, but they
aren't conceptually that difficult.  A stream is simply a conceptual
pair of objects.  The first object represents the head of a the
stream.  The second object is a _function_ which returns the rest of
the stream when called.  From these simple beginnings we can produce a
data structure with all sorts of unusual behaviors.  For instance, its
possible to create an infinitely long stream of, for instance, all the
integers starting with 1 or all the Fibonacci numbers.  Even though
such streams are conceptually infinite, we can pass then around and
operate on them almost as we would any list.  We can even do things
which seem counter-intuitive at first, for instance, mapping a
function over an infinite stream to produce a new, infinite stream.

Streams take some getting used to, but the process can be very
enlightening.  For instance, one learns quickly why Haskellers are not
as concerned about non-tail recursion when writing stream functions.
If the recursion occurs inside the "future" of the stream, you get a
free trampoline.

Laziness, however, takes special care in Emacs Lisp, because variables
are not lexically scoped by default.  This means every time you create
a lambda which you intend to be called later, you have to make sure to
explicitly `lexically-let` over it, indicating whatever variables the
lambda depends upon.  In a way, this is good, because it forces us to
think consciously about closures, which are important ideas in
functional programming. However, it can get syntactically busy to be
wrapping up things in `lexical-let` forms all the time, particularly
because we often want only to create a lexical copy of a dynamically
bound variable.

Ergo, our very first step in creating a stream library is to create a
nice form for delaying computations.  

    (defun single-symbol-list? (item)
      (and (listp item)
           (= (length item) 1)
           (symbolp (car item))))
    (defun binderish? (item)
      (and (listp item)
           (= (length item) 2)
           (symbolp (car item))))

    (defun with-form->binder (item)
      (cond ((symbolp item )(list item item))
            ((listp item)
             (cond ((single-symbol-list? item)
                    (cons (car item) item))
                   ((binderish? item)
                    item)
                   (t (error "with-forms require symbols, 
                              a single symbol list, or a binder-like 
                              expression.  Got %S." item))))
            (t (error "with-forms require symbols, 
                       a single symbol list, or a binder-like 
                       expression.  Got %S." item))))

    (defmacro* later (expr &key (with nil) (with* nil))
      (cond (with 
             `(lexical-let ,(mapcar #'with-form->binder with)
                (later ,expr :with* ,with*)))
            (with* 
             `(lexical-let* ,(mapcar #'with-form->binder with*)
                (later ,expr)))
            (t `(lambda () ,expr))))

The macro `later` takes a single expression and wraps it in a `lambda`
with no arguments.  This is exactly the sort of lambda which forms the
tail of streams.  Because the contents of the tail expression often
depend on variables dynamically bound at the time the `lambda` is
created, later lets you specify which values to produce a closure over
in several ways.

    (later 10) ;-> (lambda () 10)
    (let ((x 10))
      (later x :with (x))) -> (let ((x 10))
                                (lexical-let ((x x))
                                 (lambda () x )))
    (let ((x 10)) 
       (later y :with ((y (+ x 1))))) ->
     (let ((x 10)
        (lexical-let ((y (+ x 1)))
          (lambda () y))))
     
    (let ((x 10))
      (later z :with* ((y (+ x 1))
                       (z (+ x y))))) ->
      (let ((x 10))
        (lexical-let* ((y (+ x 1))
                       (z (+ x y)))
          (lambda () z)))

In words, `later` takes an expression and a list of binding
expressions which are similar to `let` binders, but which can be
single symbols, which expand to a `(x x)` binding expression.  Now we
are equipped to do a reasonable job implementing streams.

(Note: These streams are based loosely on those covered in "The
Reasoned Schemer", although they've been adapted to stand alone from
the Kanren interpreter and also to be more easily understood (to me)).

We could represent streams as cells, build them using "cons," etc, but
I prefer to have a bit more error detection built into the
implementation.  I don't want to accidentally use a list as a stream
or vice versa.  Emacs provides some basic facilities for defining new
"types," with `defstruct` (see Footnote 2).  

    (defstruct stream head future)
    (defun stream (hd &optional future)
      (make-stream :head hd :future future))

This defines the functions `make-stream`, `stream-head`,
`stream-future` and `stream-p`.  Good enough for use to get started,
certainly.  The function `stream` is just the stream analog of `cons`,
it creates a stream cell.  We allow the tail to be optional, because
we'll want to create streams of one element frequently.
Incidentally, `stream` with one element is our `return` operation
for the stream monad.

What about `car` and `cdr`?  Stream `car` (`scar`) is easy enough:

    (defun scar (stream)
      (cond ((not stream) nil)
            (t 
             (progn
               (if (not (stream-p stream))
                   (error "Tried to take the scar of a non-stream %S." stream))
               (stream-head stream)))))

The first cond checks for the nil stream, which we represent with
`nil`.  If the stream isn't nil, it returns the head portion of the
stream.  Easy enough.

    (defun scdr (stream)
      (cond ((not stream) nil)
            (t
             (progn
               (if (not (stream-p stream))
                   (error "Tried to take the scdr of a non-stream %S." stream))
               (let ((fut (stream-future stream)))
                 (if fut
                     (progn 
                       (if (not (functionp fut)) 
                        (error "The future of a stream must be 
                                either nil or a function.  Instead its %S" fut))
                       (let ((fut (funcall fut)))
                         (if (not (stream-p fut)) 
                          (error "The future of a stream must 
                                  evaluate to a stream.  Instead it was %S" fut))
                         fut))
                   nil))))))

`scdr` is longer, but it is simple enough.  Check for nil, in which
case the `scdr` is nil.  Otherwise, grab the lambda in the second half
of the stream and call it, checking to make sure that the output is
itself a stream.  Return that.

`defstruct` created `stream-p` for us, which tests to see if an object
counts as a stream, but we want the definition to include the `nil`
object, so we should define for later:

    (defun stream? (x) (or (not x) (stream-p x)))

I like the question-mark indicates predicate style better anyway.

It is convenient to split a stream into three cases.  The first case
is the nil stream, the second is a stream with one element, and the
third is a stream with a future.  Lots of algorithms need to act
differently in these cases, so we should write a macro over them:

    (defun stream-future-nil? (object)
      (nil? (stream-future object)))
    (defun stream-with-future? (object)
      (not (nil? (stream-future object))))

     (defmacro stream-case (stream
                            nil-case
                            =a=expressions
                            =a-f=expressions)
       (with-gensyms 
        (stream%)
        `(let ((,stream% ,stream))
           (if (not (stream? ,stream%)) 
              (error "Stream-case needs a stream 
                      input, got instead %S." ,stream%))
           (cond 
            ((nil? ,stream%)
             ,@nil-case)
            ((stream-future-nil? ,stream%)
             (let ((,(car (car =a=expressions)) (scar ,stream%)))
               ,@(cdr =a=expressions)))
            ((stream-with-future? ,stream%)
             (let ((,(car (car =a-f=expressions)) (scar ,stream%))
                   (,(cadr (car =a-f=expressions)) (stream-future ,stream%)))
               ,@(cdr =a-f=expressions)))
            (t (error "Couldn't figure out what to do 
                       with stream %S.  This should never 
                       happen." ,stream%))))))

Stream case figures out which condition our stream is in, and then
destructures the stream into the appropriate parts.  When the stream
is nil, the first body form is simply executed.  When the stream is a
singleton, the value contained in the stream is bound to a symbol the
user passes in and the bodyforms are executed in that context, and
when there is a value and a future-stream, those are bound to user
defined symbols in a body for that case.  It is all wrapped up in a
hidden `cond`.

Example:

    (let ((s (stream 'x (later (stream 'y nil)))))
      (stream-case s
        ((print "this is not executed because s is not nil")
        ((a) (print "If the stream had a single element, a would be
      bound to it here"))
        ((a f) 
         (print "This case is executed becase s has a future, a is the
      `scar` of s, f is the function which produces the future
      stream.")))))

About the simplest function we can write acting on streams is
`take-n`, which simply takes a limited number of elements from a
stream and converts them into a list.  

    (defun take-n (stream n &optional acc)
     (if (= n 0) (reverse acc)
       (stream-case stream 
         ((reverse acc))
         ((a) (reverse (cons a acc)))
         ((a f) (take-n 
                 (funcall f) (- n 1) (cons a acc))))))

This function will return fewer than the requested number of elements
if the stream ends before `n` is reached.  Readers might notice this
is a tail-recursive function.  Emacs doesn't support tail-recursion
natively, but I wrote a library that does, so we can define this
function this way:

    (require 'recur)
    (recur-defun* take-n (stream n &optional acc)
      (if (= n 0) (reverse acc)
        (stream-case stream
                     ((reverse acc))
                     ((a) (reverse (cons a acc)))
                     ((a f) (recur (funcall f) (- n 1) (cons a acc))))))

This will never blow the stack, which is important, since we want to
take many elements off a stream without worrying about whether the
stream is shorter than the recursion limit.

Ok.  With just these few functions we can play with some non-trivial
streams.  

    (defvar *ones*
       (stream 1 (later *ones*))
       "Infinite stream of ones.")

    (defun ints-from (n)
      (stream n (later (ints-from (+ n 1)) :with (n))))

    (take-n (ints-from 5) 10) ;-> (5 6 7 8 9 10 11 12 13 14)
    (take-n (ints-from 10) 10);-> (10 11 12 13 14 15 16 17 18 19)

These are really just parlor tricks, though.  See if you can figure
out how to define `forever,` which is a function which takes a value
and returns a stream of that value forever.  Or `repeating,` which
takes a list and returns a stream which is that list repeated over and
over again.  

Note that even though functions like `ints-from` appear to call
themselves, they do so only _after_ they return their stream.  There
isn't any real recursive call - the stream serves as a trampoline.
These functions won't blow the stack.

Building Towards the Stream Monad
---------------------------------

Streams are obviously somewhat analogous to lists.  Lists form a monad
with `list` and `map-cat` as the return and bind operations
respectively.  We'd like to form a monad with streams too.  Then we
could do some truly interesting things with `mlet*` like notation,
cuing up potentially infinite calculations into a nice list-like
package.  

We've already determined that the `return` operation for a stream
monad would simply be the function which puts a single value into a
singleton stream:

    (defun stream-return (x) (stream x))

We need to build up to `mapcat` for streams, and then we will have our
monad.  

Streams get a little confusing at this point.  The key to keeping our
head straight is to remember that the tail of the stream _always_
needs to be handled in such a way as to maintain the laziness of the
stream.  When you `map` over a stream, you don't actually visit the
rest of the stream at the time of the `map`.  You simply return a new
stream whose future includes the fact that it needs to apply a
function to each value before returning it.  `stream-map` is just as
lazy, in other words, as our streams.

Let's write `smapcar`, that is `stream-map-car.`  The exact analog of
the list `mapcar` function.  It will take a function and a stream and
return a new stream which is that function applied to every element in
the input stream.

    (defun smapcar (f stream)
      (stream-case stream
        (nil) ; empty stream -> empty stream
        ((a) ; singleton stream -> singleton stream of (f a)
         (stream (funcall f a) nil))
        ((a future)
         (stream (funcall f a)
           (later (smapcar f (funcall future)) :with (f future))))))

The only case which might be confusing is the last case.  The
`stream-case` expression extracts `a`, the value at the head of the
stream, and `future`, the function which returns the future of the
stream.  We form a new stream whose head is `(f a)` and whose future
is the mapping of `f` onto the `future` of the input stream.  This
mapping occurs `later`, only when someone asks for the future of the
output stream.

It is possible to write a generalization of this function which maps a
multi-input function over multiple streams.  I'll leave such a
function to the reader, but I will provide here a function `smapcar2`
which maps over two streams, because it will let us construct some
interesting streams.

    (defun smapcar2 (f-of-2 stream1 stream2)
      (stream-case stream1
                   (nil)
                   ((a) (stream-case stream2
                         (nil)
                         ((b) (stream (funcall f-of-2 a b) nil))
                         ((b g)
                          (stream (funcall f-of-2 a b) nil))))
                   ((a f)
                    (stream-case stream2 
                     (nil)
                     ((b) (stream (funcall f-of-2 a b) nil))
                     ((b g)
                       (lexical-let ((f-of-2 f-of-2)
                                     (f f)
                                     (g g))
                        (stream (funcall f-of-2 a b) 
                         (lambda () (smapcar2 f-of-2 (funcall f)
                          (funcall g))))))))))

With `smapcar2` we can finally create things like the stream of
Fibonacci Numbers:

    (defvar fibs (stream 1
                          (later 
                            (stream 1 
                                    (later (smapcar2 #'+ fibs (scdr fibs))))))
       "The stream of Fibonacci numbers")
    (take-n fibs 10) ;-> (1 1 2 3 5 8 13 21 34 55)

Whee!  An eternal golden braid, etc!

Stream Map & Concatenate 
------------------------

We might think that all we need to do now is define `stream-map` and
`stream-cat` and then `stream-map-cat` would be the composition of
these functions.  This might almost be made to work except that either
operation might be produce an infinite stream of results.  It is more
straightforward to interleave the concatenation with the map
operation.  We can define `stream-cat` by itself:

    (defun stream-cat (stream1 stream2)
      (stream-case 
       stream1
       (stream2)
       ((a) (stream-case stream2 
                         ((stream a nil))
                         ((b) (stream a (lexical-let ((b b)) 
                                          (lambda () (stream b nil)))))
                         ((b g)
                          (stream a (lexical-let ((b b)
                                                  (g g))
                                      (lambda () (stream b g)))))))
       ((a f)
        (stream-case stream2
                     ((stream a f))
                     ((b) (stream a 
                                  (lexical-let ((f f)
                                                (stream2 stream2))
                                    (lambda () (stream-cat (funcall f) stream2)))))
                     ((b g) 
                      (stream a 
                              (lexical-let ((f f)
                                            (stream2 stream2))
                                (lambda () (stream-cat (funcall f) stream2)))))))))

This function is a doozy (see Footnote 3), but the upshot is simple.  If either stream
is empty, we return the other stream, obviously.  Otherwise we lazily
pass stream two down stream one, until we find the tail of stream one,
whereupon we fasten stream two.  A call to stream cat doesn't
instantaneously modify the entirety of `stream1`, it should be noted.
It modifies just the tail, essentially passing instructions "down the
line" to further modify each tail until a `nil`.

We can now define `stream-map-cat`.

    (recur-defun* stream-map-cat (mf stream)
      (lexical-let ((mf mf))
        (stream-case stream
                     (nil)
                     ((a) (funcall mf a))
                     ((a f) 
                      (lexical-let ((interior-stream (funcall mf a))
                                    (f f))
                        (stream-case 
                         interior-stream
                         ((recur mf (funcall f)))
                         ((b) (stream b 
                                      (later
                                       (stream-map-cat mf (funcall f)))))
                         ((b g) (stream b
                                        (lexical-let ((g g))
                                          (later 
                                           (stream-cat (funcall g)
                                                       (stream-map-cat mf (funcall f)))))))))))))
   
`stream-map-cat` is actually pretty easy to understand if you remember
that `mf` is going to be a monadic function of the stream monad, and
so whatever is passed _in_, `mf` always returns a stream.  Obviously
if the input stream is `nil`, the output is also.  If input has a
single element, then `mf` on that element is the output stream.   When
there is a non-trivial stream, then we evaluate the monadic function
to get the `interior-stream` and lazily concatenate it on the tail of
the result of `map-cat` on the rest of the input stream.

That is it.  We have the stream monad:

    (setq monad-stream 
          (tbl! :m-bind #'stream-bind
                :m-return #'stream-return
                :m-zero nil))

So what can we do with it?
--------------------------

We'll, besides implement Kanren, the point of all this, I can think of
at least one interesting example.  Emacs provides functions to
generate random numbers with a uniform distribution.  `random*` from
the `cl.el` library is able to produce such numbers in any given
range.  Consider the function:

    (defun* random-numbers (lim &optional (state (make-random-state)))
      (let* ((*random-state* state)
             (val (random* lim)))
        (lexical-let ((new-state (make-random-state))
                      (lim lim))
          (stream val (lambda () 
                        (random-numbers lim new-state))))))

This function takes a limit and an optional state and returns a stream
of uniformly distributed numbers.  Using a dynamically bound
`*random-state*` and the fact that `make-random-state` returns the
current state, the resulting stream returns an infinite, but
repeatable, list of uniformly distributed numbers.  

Uniform distributions are fine, but we often want Guassian numbers.
There are several ways to get them if you have uniform distributions.
One way is to take the average of many uniformly distributed numbers.
These averages will have a Gaussian distribution which can be scaled
to whatever standard deviation is desired.  A way which is less
computationally intensive, however, is the Box-Miller transformation,
which takes two independent distributions of uniformly distributed
numbers and returns two distributions of Guassian numbers.  If `u` and
`v` are uniformly distributed, then 

    r = sqrt(-2*log u)*cos(2 pi v)
    s = sqrt(-2*log u)*sin(2 pi v)

`r` and `s` are two independent but _normally_ distributed numbers.
If we don't care about the seeds in particular, only that they are
different, we can create a stream of Guassian numbers like so:

    (defun zip-streams (&rest streams)
      (apply #'smapcar* #'list streams))


    (setq normal-numbers 
          (mlet**_ monad-stream ((pair
		              (zip-streams (random-numbers 1.0)
                                   (random-numbers 1.0
                         (make-random-state t)))))
                   (lexical-let ((u (car pair))
                                 (v (car pair))
                                 (r (sqrt (* -2 (log  u))))
                                 (s (* 2 pi v)))
                     (stream (* r (cos s))
                             (later (stream (* r (sin s)) nil))))))

The monad handles collecting the two results into a single stream for
us, and the result is itself an infinite list of normally distributed
numbers.  I don't know about you, but I think that is pretty cool.

Don't Interleave Just Yet
-------------------------

Because streams can be infinite, sometimes we don't want to strictly
concatenate them.  We can certainly concatenate a stream of infinite
ones and a stream of infinite twos, but the result will be,
effectively, a stream of infinite ones.  The twos will be conceptually
at the end of this stream, but you'll never reach them.  Because of
this, we sometimes want to _interleave_ the results of monadic
functions.  Hence, this library provides an alternative monad
`monad-stream^i` which has this behavior.  In this case `i` means
"interleave" rather than `implicit.`  I'm going to have to do
something about that.

The Peg Puzzle
--------------

If you return to the peg puzzle, making the minor changes you need to
adapt the functions which enumerate possible moves so that they return
streams, you can use almost the exact same code to produce a lazy-list
of all possible solutions to the peg game.  The full code is available
in the `examples` directory.  Using this code, one can `queue up` all
the solutions to the puzzle, actually examining and returning only
those which are needed.

I've implemented a slightly more complex version of the peg puzzle
problem so that we end up with not just a stream of solved puzzles,
but a stream of solved puzzles AND the included solution as a list of
instructions.  It's too length to reproduce here, but you can see the
code [here][peg-puzzle].



As always, all the code here, and this tutorial, are available from my
[github account][emacs-utils].

* * *

Footnote 1: I've found it handy define a set of "implicit" monads, the
defining feature of which is bind does a lot more heavy lifting,
basically squeezing non-monadic values into the monad if it encounters
them during its work.  For instance, the implicit list monadic bind
operation takes two arguments.  When the first is a list, bind is
identical to the non-implicit list monad, but when it isn't a list it
is wrapped in one before things proceed.  Similarly, monadic functions
may return non-lists, and when this happens, they are simply loaded
into lists before being treated normally.  The end effect is that you
don't have to decorate all your expressions with `m-return` all the
time.  The only time you need to use one is when you really mean to
return a `list of nil` rather than `nil` itself.

This might be a crazy thing, but seems ok to me!  It seems to be more
appropriate for dynamically typed languages because functions are
always modifying their behavior to fit the input types anyway.

Footnote 2: Deftype produces a type which is unfortunately true under
`vectorp`.  Too bad. 

Footnote 3: 
The function shown here is technically correct, in terms of the final
values that appear in the stream and where.  However, it is not as
lazy as it could be.  The key recognition is that, while `mf` is not
the tail of a stream, the partial application of `mf` to `a` is a
function which takes no arguments and returns a stream.  That is,
`(lambda () (funcall mf a))` is an acceptable stream tail, and we
don't need to evaluate `mf a` to use it as such.  The following code
implements this fully lazy version of `stream-map-cat`, here called
`stream-map-cat-tail`.  The only thing you need to understand in
addition to the above is that `par` in the following code stands for
`partially apply on the righ`.  

    (defun stream-cat-tail (head-stream tail)
      (if (stream? head-stream)
          (stream-case head-stream
                       ((funcall tail))
                       ((a) (stream a tail))
                       ((a f)
                        (stream a 
                                (later 
                                 (stream-cat-tail (funcall f) tail) :with (f tail)))))
        (stream-cat-tail (funcall head-stream) tail)))

    (defun* stream-map-cat-tail (mf instream)
      (stream-case instream
                   (nil)
                   ((a) (funcall mf a))
                   ((a f)
                    (let ((tail (par mf a)))
                      (stream-cat-tail tail
                                       (later
                                        (stream-map-cat-tail mf (funcall f))
                                        :with (mf f)))))))

`stream-cat-tail` pins the function `tail` at the end of the stream
`head-stream`.  It turns out to be convenient to pass functions
returning streams into the `head-stream` position, so this function
checks for that case and extracts the head stream if needed.  The
library actually uses these functions rather than the slightly less
lazy version in the tutorial body.  I found that version easier to
understand at first.

[emacs-utils]: https://github.com/VincentToups/emacs-utils "Emacs Utils"
[peg-puzzle]: https://github.com/VincentToups/emacs-utils/blob/master/examples/peg-puzzle-streams.el "Peg Puzzle"
