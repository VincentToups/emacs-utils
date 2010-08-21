How to use a codewalking macro to implement a lexically scoped `delay` in Emacs Lisp 
------------------------------------------------------------------------------------
> Vincent Toups

Lazyness is all the rage in these days of Haskell and Clojure, and it
comes in many forms.  Haskell is lazy by default, clojure supports
laziness extensively, but it is still voluntary.  Some scheme
implimentations provide _promises_, which are delayed computations,
and almost every high level programming language provides the basic
building block of laziness via closures and anonymous functions.  

We might, for instance, implement laziness in Scheme with a macro
which transforms:

    (delay expr1 ... exprN) 

to 

    (lambda () expr1 ... exprN)

and then `force` is simply implemented as a function call:

    (define (force thunk) (thunk))

Something which I think is a key insight here is that _lambda is
intelligent quotation_.  I'm not a programming language theorist, so I
don't know if this constitutes a particularly novel insight, but to me
it was a crucial step towards understanding the semantics underlying
most programming languages.  What does "lambda is intelligent
quotation" mean, though?  Well, we can imagine writing a very
primitive `delay` macro thusly (in some kind of lisp):

    (defmacro (delay . args) `(quote (begin ,@args)))

and then `force` is just `eval`:

    (define force eval)

While this certainly "gets the job done" for expressions containing
only atoms, it doesn't work on any expression containing variables.
Depending on how your Lisp works, and when you call `force`, you can
get anything from a straight up error to a mysterious result.  If you
use a `lambda` instead, however, you tell your language "quote this
expression, but make sure all the symbols in the expression refer to
the values described by the lexical context."  

So here is a question: if we want to implement `delay` and `force` in
emacs lisp, how do we do it?  An extremely naive guess would be just
to use `lambda`:

    (defmacro delay (&rest body) `(lambda () ,@body))

Force is still:

    (defun force (thunk) (funcall thunk))

But this doesn't quite work as we expect.  


    (let ((thunk 
      (let ((x 10))
        (delay (+ x x)))))
          (force thunk))

This will produce an error - that `x` is undefined.  This is because,
vexingly, Emacs Lisp is an old Lisp and has dynamic scope.  That means
`x` in the lambda created by delay refers to whatever the outermost
`x` binding is, which, by the time we've arrived at the  `force` is
just nothing.  

"Fear not," I hear you say!  "I've read that Emacs Lisp has lexical
closures via the cl.el package.  Why don't we just use those?" 

The item in question is `cl.el`'s `lexical-let` form.  `Lexical-let`
lets you do things like 

    (funcall (lexical-let ((x 10))
               (lambda () (+ x 101)))) ; -> 111

It uses some trickery which I frankly haven't gotten too deeply into
to simulate a lexical closure so that lambda grabs the right variable
in the body of a `lexical-let`.  However, note that we have to
enumerate the variables we'd like to close over.  We could write a
macro to the effect of `delay-with` which had a body and a list of
variables to close over, but can we do better?

We can, in fact, do better with a `codewalking macro`.  Most macros
which take a "body" do something simple.  A good example is emacs
lisp's `save-excursion`, which basically just inserts code to save and
restore the current editing state around an unmodified body.  

A codewalking macro takes a set of lisp forms, walks over them,
inspecting and transforming, and finally generates a perhaps radically
transformed new set of lisp expressions.  We can write a codewalking
macro to implement `delay`.  All it has to do is walk over the body of
the code and collect all the symbols and functions that are lexically
free in it, and then lexically close around these symbols.  Then, as
if by magic, delay becomes a "smart" quotation.

So if our body is something like `(+ x y z)`, then our job is pretty
easy: `x`, `y`, and `z` are obviously free variables in the
expression, so our macro takes:

    (delay (+ x y z))

to

    (lexical-let ((x x)
                  (y y)
                  (z z))
      (lambda () (+ x y z)))

If we put this in context:

    (setq thunk (let ((x 10) (y 11) (z 12))
                  (delay (+ x y z))))
    (force thunk) ; -> 33

Note that above the `let` expression forms a dynamic scope, but our
delay macro calculates the free variables and lexically closes over
them before the dynamic values are popped of the stack.  Then, when
`thunk` is called, everything works out all right.

A couple of objections ought to be appearing in your mind right now.
One of them focuses on the fact that lisp expressions are not as
simple as just function calls, and that, in particular, a variety of
lisp expressions introduce variable bindings which influence nested
expressions.  Most obviously,

    (setq thunk (let ((y 1001) 
                      ((x 10)))
                (delay (let ((x (+ x 100)))
                      (+ x y)))))
    (force thunk)

involves some thinking about what values we should close over.  Rather
obviously we need to capture the lexical binding of `y`, but what
about `x`.  Within the body of the inner `let` expression, `x` will
be dynamically bound, but the value it takes depends on the
non-lexically bound outer `x`.  If we traversed the code tree, just
counting `x` occurences, we might miss the fact that `let` treats the
symbol part of its binding expressions differently than the value
parts.  Not only that, but we have to consider the dynamic and lexical
binding of function applications, which are treated separately in
emacs lisp because it is a lisp-2.  This problem is tractable,
however, because emacs lisp has a small number of simple fundamental
expression types.  We can, therefore, hope to write a parser which
traverses arbitrary lisp code, collecting information about which
variables are locally bound or lexically free within an expression.

Here comes the second objection, however.  It is true that lisp
defines only a small number of fundamental special forms, but any
arbitrary piece of lisp code may be peppered with a variety of user
defined special forms created via macros.  It may be within our
capabilities to parse a lisp expression containing only fundamental
forms, but can we reasonably expect ourselves to parse the binding
information contained in the clauses of a `loop` expression, much less
any unique, ad-hoc, user created macro?  The good news is that we
don't have to - we can call `macroexpand-all` on the body before
parsing it, guaranteeing that we will have only naked lisp to parse.

Now that those objections are out of the way, we can begin to tackle
the problem formally.  We'd like to write a function which reads a
piece of lisp code and and records the number of times a symbol is
used without a binding, either as a function or as a value.  Once we
have this list we can wrap up our expanded body in `lexical-let` and
`labels` forms, to provide local definitions of functions and values
which are lexically scoped.

The skeleton to collect the symbol usage information is, thus;

    (defun* collect-usage-info (form &optional (global-info (alist>>))
                                     (local-info (alist>>)))
    "(collect-usage-info FORM &optional GLOBAL-INFO LOCAL-INFO)

    Collects information about the usage of variables and functions in the form FORM.
    When called recursively, it may be appropriate to provide GLOBAL-INFO, an alist 
    describing the collected information so far, and LOCAL-INFO, an alist describing
    the binding information of variables locally.  

    GLOBAL-INFO is returned.  It is an alist of the form;
      (list 
        (<symbol> (<n-bound-symbol-usages>
                   <n-bound-function-usages>
                   <n-unbound-symbol-usages>
                   <n-unbound-symbol-usages>))
        ...)

    LOCAL-INFO is recursively defined, but is of the form
      (list 
        (<symbol> (<bound-as-symbol>
                   <bound-as-function>))
        ...)

      Where bound-as-(function/symbol) are either t or nil."
      (cond
       ((symbolp form)
        (cond ((eq 't form) global-info)
              ((eq 'nil form) global-info)
              (t
               (dlet [[bound-s bound-f] (symbol-binding-info form local-info)
                      [s-count f-count s-unbound-count f-unbound-count] (symbol-bind-counts form global-info)]
                 (alist>> global-info form (list 
                                            (if bound-s (+ 1 s-count) s-count)
                                            f-count
                                            (if (not bound-s) (+ 1 s-unbound-count) s-unbound-count)
                                            f-unbound-count))))))
       ((listp form)
        (cond 
         ((quotep form)
          global-info)
         ((prog-like form)
          (collect-usage-info-prog-like form global-info local-info))
         ((letp form)
          (collect-usage-info-let form global-info local-info))
         ((let*p form)
          (collect-usage-info-let* form global-info local-info))
         ((function-formp form)
          (collect-usage-info-function form global-info local-info))
         ((ifp form)
          (collect-usage-info-if form global-info local-info))
         ((condp form)
          (collect-usage-info-cond form global-info local-info))
         ((defunp form)
          (collect-usage-info-defun form global-info local-info))
         ((setqp form)
          (collect-usage-info-setq form global-info local-info))
         (t ; function application
          (collect-usage-info-prog-like (cdr form)
                                        (collect-usage-info-function `(function ,(car form)) global-info local-info)
                                        local-info))))))

Anyone familiar with SICP will recognize this pattern.  We have a case
statement which determines what sort of expression we are looking at,
and then dispatches handling of that expression type to a specific
handler, which may call `collect-usage-info` recursively.  The base
cases are symbols and atoms.  Atoms are ignored, whereas symbols, when
encountered, are counted appropriately depending on the information in
`local-info`.  You can see all the details in the source code,
available at my git-hub, but to give you an idea of how this works,
lets look at the case of the `let` expression.  

We detect a let-expression with the `letp` predicate;

    (defun letp (form) 
       (and (non-empty-listp form)
            (eq (car form) 'let)))

(all these predicate functions are defined in `macro-utils.el`.)

Handling a `let` form is relatively simple.  We will read each binding
clause, count the variable usage in the expression providing the value
thereof, and then collect all the symbols bound in the expression.  We
then call `collect-usage-info-prog-like` on the body, providing an
augmented `local-info` which indicates that the variables described in
the `let` expression now have bindings.
`collect-usage-info-prog-like` simply folds `collect-usage-info` over
the forms in the body.

This idea is repeated for all forms in the basic emacs-lisp language.
Using this function we can finally define our delay macro thusly:


    (defmacro* delay (&body body)
      (let* ((expanded (macroexpand-all `(progn ,@body)))
             (info (collect-usage-info expanded))
             (unbound-symbols (get-unbound-symbols-list info))
             (unbound-functions (get-unbound-function-symbols-list info))
             (temporary-function-names 
               (loop for f in unbound-functions collect (gensym (format "%s-old-" f)))))
               `(lexical-let ,(loop for s in unbound-symbols collect `(,s ,s))
                              (labels ,(loop for old-f in temporary-function-names 
                                              and f in unbound-functions collect 
                                              (let ((arglist (gensym "arglist")))
                                                `(,old-f (&rest ,arglist) (apply #',f ,arglist))))
                                       (labels ,(loop for f in unbound-functions 
                                                       and old-f in temporary-function-names collect 
                                                       (let ((arglist (gensym "arglist")))
                                                         `(,f (&rest ,arglist) (apply #',old-f ,arglist))))
                                                (lambda () ,expanded))))))
    


As promised, there isn't much by way of magic here.  We expand the
`body` as a progn-like form, collect the information about variable
binding from it, and then create the appropriate lexical closures.
The only wrinkle is in the precise use of the `labels` form.
Functions introduced in a `labels` form are allowed to be mutually and
self recursive, so we can't just do the equivalent of:
    
    (let ((x 10))
       (lexica-let ((x x))
           (+ x x)))
    

which is,
    
    (flet ((f (q) (+ q 1)))
      (labels ((f (&rest arglist) (apply #'f arglist)))
          (f 1)))
    
Because the labels form above introduces a recursive function `f`
which calls itself indefinitately.  There are several possible
solutions to the problem.  The simplest, but perhaps not the best, is
to use two independent `labels` forms, the first of which creates new
bindings for the functions, and the second of which creates the
lexical bindings with the correct names.  Perhaps the ultimate
solution is to pick new names, and walk the code, replacing each
function application with the new name.  This is probably overkill for
this example.
    
So we've finally arrived!  Now we can delay and force to our heart's
content, and emacs will do its best to pretend it is a lexically
scoped language:
    
    (let ((x 10))
       (setq thunk (delay (+ x 1))))
	(force thunk) ;-> 11 
	x ;-> undefined variable 

As an exercise to the reader, you can try to implement two functions
which are provided in the source code, but not reproduced here:
`lexical-lambda` and `lexical-defun`.  They have the following
behaviors.


    (funcall 
      (let ((x 10))
        (lexical-lambda (y) (+ y x)))
      100) ; -> 110


    (let ((x 10))
      (lexical-defun f (y) (+ x y)))

    (f 10) ;-> 20 

