Monadic Parser Combinators, in Emacs Lisp
-----------------------------------------

I've put together rudimentary (at the moment underdocumented) support
for a parser monad in `monad-parse.el`.  This library is something of
a shambling mongrel.  Obviously, it is in emacs lisp, but it is built
on top of my implementation of monads and destructuring bind from
Clojure, but it is based on [a monadic parser combinator
library called Smug](http://common-lisp.net/~dcrampsie/smug.html) implemented by
Drew Crampsie in Common Lisp.

This was a tricky thing to get right, even with all the plumbing
provided by the code in `monads.el` because monadic parsers depend a
lot on lexical scope, which can be simulated in emacs lisp, but you
have to do it explicitely.  

Things are basically just like Drew Crampsie's library except that
I've used my `domonad` form to support his `=let*` form, and as a
consequence the binding forms in that expression follow the clojure
style, rather than the Common Lisp/Emacs Lisp style.  This means that
his `results` are my `returns,` and for simplicity I provide
`parser-bind` and `parser-result` global function bindings.  Lots of
the functions in Smug have a `parser-` prefix because Emacs Lisp lacks
a good namespace mechanism.  `=let*`, in a giant gotcha, automatically
applies `parser-return` to its `body`, so you don't need to indicate
`return` when using it.  Besides that, its very similar.

For instance, using Smug, `zero-or-more` looks like:

    (defun zero-or-more (parser) 
      (=or 
        (=let* 
         ((x parser) 
          (xs (zero-or-more parser))) 
         (result (cons x xs))) 
        (result nil))) 

In this library, which lacks a snappy name because it is too
frankensteinish and slow to be really usable (probably), this would
be:

    (lex-defun zero-or-more (parser) 
      (=or 
        (=let* 
         [x parser
          xs (zero-or-more parser)]
         (cons x xs))
        (parser-return nil)))

The differences are: Because `=let*` is producing a function, you need
a lexical closure over `parser`, so I use my `lex-defun` form to
create it.  The binding forms in `=let*` use `[]` and no internal
delimiters, and the body of `=let*` doesn't require a `parser-return` function.

`=let*` is (for reference) literally implemented as:

    (defmacro* =let* (bindings &body body)
     `(domonad monad-parse ,bindings ,@body))

I may provide a Smug compliant version of `=let*` eventually.

One other major difference is that you've got to jump through a hoop
to support generic input types.  I've used
[eieio](http://cedet.sourceforge.net/eieio.shtml) to provide the
interface for a parsing input stream.  Right now, only strings are
supported as parsing streams, but I want to be able to add parsing buffers
cheaply in the future.  As a consequence, I've wrapped a string up in
an eieio class `<parser-input-string>` with methods `input-empty?`,
`input-empty-p` (synonyms), `input-rest`, and `input-first`.  Because
eieio doesn't cover the whole emacs class universe, you've got to wrap
a string before using it via `string->parser-input.`

Reading about [Smug](http://common-lisp.net/~dcrampsie/smug.html) is
probably a great place to start if you want to understand this
library, with the above provisos.  Here is an example, though:

    (lexical-let ((digits (coerce "1234567890" 'list)))
      (defun digit-char? (x)
        (in x digits)))

    (lexical-let ((lowers (coerce "abcdefghijklmnopqrztuvwxyz" 'list))
                  (uppers (coerce "ABCDEFGHIJKLMNOPQRZTUVWXYZ" 'list)))
      (defun upper-case-char? (x)
        (in x uppers))
      (defun lower-case-char? (x)
        (in x lowers)))

    (defun =char (x)
      (lexical-let ((x x))
        (=satisfies (lambda (y) (eql x y)))))
    (defun =upper-case-char? ()
      (=satisfies (lambda (y) (upper-case-char? y))))
    (defun =lower-case-char? ()
      (=satisfies (lambda (y) (lower-case-char? y))))

    (defun =digit-char ()
      (=satisfies #'digit-char?))

    (defun letter () (parser-plus (=lower-case-char?) (=upper-case-char?)))

    (defun alphanumeric () (parser-plus (=digit-char) (letter)))

    (funcall (zero-or-more (alphanumeric)) (string->parser-input "aaaa?"))


Have fun!

Disclaimer: This library depends on so much insanity that I cannot
guarantee that it will function as advertised or that it will not
make you lose your mind.

Notes on the monad.
-------------------

This library is obviously monadic, but what is the monad?  Well, it
its the parser monad.  Monadic values are functions which accept a
value and return a parser, which is itself a function which accepts an
input and returns a list of possible parsing results, which are
value/post-input pairs.  It is useful to examine the return operation
of any monad to understand what the monadic values are (in statically
typed languages, you can also see the contained type in the type of
the input argument.  In our case, its just "lisp values".)

    (defun parser-return (val)
      (lexical-let ((val val))
        (lambda (input) 
          (list (cons val input)))))

Return takes a value, and returns a (monadic) function (of the list of
value/input pairs monad, as an aside).  The type of this function is
input -> (list Pair).  Ok, these functions from inputs to
value/post-input pairs are MONADIC VALUES.  What are MONADIC
FUNCTIONS?

Well, they are functions like BIND.  They take a value and return a
MONADIC VALUE.  That is, they are functions of values which return
functions of inputs which return lists of pairs of values and inputs.




