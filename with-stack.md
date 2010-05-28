with-stack.el - An embedded stack language for Emacs Lisp
---------------------------------------------------------

# Introduction #

`with-stack.el` implements a stack based language inside emacs lisp.
The language will be familiar to anyone who knows Forth, and more
familiar to anyone who knows factor, on which it is more closely based
(and towards which it will develop, although for various reasons it
will never really converge with).

With stack lets you write something like this:

    (setq x (||| 1 1 2>+))	 

Which should be read as a regular emacs expression until the form
`|||` which tells emacs to start acting like a stack language.  The
rest of the form reads, in words, "push 1 on the stack, push 1 on the
stack, and then take two arguments off of the stack and pass them to
the emacs lisp `+` function, pushing the result onto the stack.
Finally, return the top of the stack."

One concession to the emacs environment is that the emacs lisp stack
language uses the emacs lisp reader.  Numbers, strings, and vectors
are indicated the same way as in elisp, and pushed onto the stack as
they are encountered.  Quoted objects are pushed onto the stack also.
There is one wrinkle, however.  the syntax `n>symbol` instructs the
stack language to take n arguments from the stack and pass them to the
elisp function indicated by the symbol, if n is a number.  If n is
literally the symbol `n`, then the stack language reads the top of the
stack (at runtime) to determine how many arguments to grab.  For
instance.

    (||| 1 2 3 4 3>list) -> (2 3 4)

While 

    (||| 1 2 3 4 5 1 1 + n>list) -> (4 5)

This makes it easy to call emacs functions from the stack language
without writing a lot of glue code.

The stack language supports lambda-like quotations, inspired by
Factor.  A quotation is just a list, constructed in any way you want,
usually by direct quotation:

    (||| 1 '(3 +) call) -> 4

I really love Factor's approach, so you'll find that quotations play a
big role in how the stack language works.  The `if` statement, for
instance, uses quotations:

    (||| 5 10 2<< '('true) '('false) if) -> 'true

but 

    (||| 5 10 2<> '('true) '('false) if) -> 'false

In other words, if expects the stack to have a condition, a true
branch quotation, and a false branch quotation to be on the stack, and
it calls the appropriate branch.

I've snuck in a feature here you might not have noticed: `if` is not
an emacs lisp function.  It is a stack language word.  You will
eventually be able to write words in emacs stack language, but for now
they are defined by using emacs lisp.


    (defstackword stack-plus 
      (let ((arg1 (pop *stack*))
            (arg2 (pop *stack*)))
         (push (+ arg1 arg2) *stack*)))

Note that with the current implementation, you can sort of write
words in the stack language itself using the emacs macro `|||-` (as
opposed to `|||`).  Whereas `|||` introduces a stack language form
with a fresh, empty stack (and retain stack, for those who care),
`|||-` uses the stack currently in the dynamic scope.  So we could
define the "push to the retain stack" word in two ways:

    (defstackword >r 
       (push (pop *stack*) *retain-stack*))

(using pure emacs lisp) or:

    (defstackword >r 
       (|||- '(push) swap 1>list compose '(*retain-stack*) compose
       1>eval drop))

Here we use lists and the compose operator to construct an emacs lisp
phrase and pass it to eval.  Not the most obvious way of doing things,
but a nice example.

When the language encounters a symbol during compilation, it first
checks to see if there is a stack word associated with it, _then_ it
tries to expand it into an emacs lisp call.  If it can't do either, an
error is thrown.  The compiler is smart enough so that you can define
a stack-language word `+` and still use the `n>+` syntax to access the
emacs function.  There are a few stack words defined in the code now
(`swap`, `dup`, `drop`, `etc`) and some words for working with
quotations (`curry`, `compose`), and some words for doing control
(`if`, and `loop`).  Where possible, I am going to hew pretty close to
Factor's style, not Forth's.

# Future #

I'd like to implement most of Factor's core vocabulary (where
appropriate) and then I really want to implement the `fry` word as
well as some functional things like `map` and `fold`.

# Updates #

28 May 2010: added a bunch of stack-words.