Emacs Stack Language
--------------------

Have you ever wanted to be as cool as Slava Pestov, all without ever
leaving Emacs?  Do you want to write code that looks like this:

	: wrap-interactive ;; ( interactive-sig name  --  ) 
	  retain> retain> retain-dup 'defun <retain retain-swap '(&rest args)
      '(interactive) <ret '(loop for i in (reverse args) do
      (esl-push i)) '(esl-do) <retain suffix 6 nlist eval-with-emacs
	  end-word:

Now you can, with the Emacs Stack language.  

Seriously, I implemented a compiled stack-based language tightly
coupled with emacs lisp.  It is in the very early stages but you can
write non-trivial code with it and it supports some neat features
(which may or may not actually work or matter).  The language is more
Factor inspired that Forth inspired, but it also features many
"concessions" to Emacs Lisp, since it is meant to be used to extend
emacs.

Code looks familiar to any Forther or Factor programmer:

    : incr ;; ( n -- n+1 )
      1 + end-word:

One major concession to emacs is that the Emacs Stack Language (ESL)
reader is the Emacs Lisp reader.  As a consequence, ";" must be the
character that begins a comment, and so words need to end with
"end-word:" instead of ";".  

You can use ESL directly in a lisp file by typing:

    (esl-dop 1 incr 2 * print)

(which prints 4).  esl-dop stands for "do some esl and then print the
stack".  Note that we can't use "." either, since it indicates an
improper list ending.  

ESL supports infinite recursion:

   

