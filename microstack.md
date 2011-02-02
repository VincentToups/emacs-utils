Microstack
----------
> Inspired by Vi's command mode but with an emacsy flavor.

Introduction
------------

Microstack is sort of like Vi's command mode, in that it turns a
series of single letter strokes and numerical (and string) arguments
into actions in the current buffer (or computations).  

On the other hand, its just a simple wrapper which lets you write a
subset of my other tool, the emacs stack language, in a highly concise
form meant to make text based tasks easier to perform.  

It was partly inspired by my own yen for the old Vi command mode,
which I abandoned after switching to emacs, and by a desire to capture
more utility for my stack language.  

What does it look like?
-----------------------

    M-x do-microstack 

Will prompt you to enter a string.  If you type

    4D"test"i

You will find that emacs has deleted the four characters ahead of your
position in the buffer, and then inserted the text "test".

If you enter instead

    "k"q10N

You'll find emacs has deleted the last ten characters.

    "dogs and cats"s4D"bees"i

Will jump forward to the first location of "dogs and cats" after the
cursor, delete dogs, and replace it with bees.

Spaces are ignored except when they separate numbers, so all the above
can be rewritten with extra spaces, if you want to.

What the hell is going on here?
-------------------------------

Microstack takes a specially formatted string, parses it into a series
of single-character "operands," numbers or strings and then passes
that sequence through a translator which transforms symbols into
stack-language words using a dictionary.  

The idea is to provide the most useful/common stack language words for
text manipulation as single character operands, for brevity, while
allowing the full power of the emacs stack language. 

How can I learn more about this amazing invention??
---------------------------------------------------

The emacs stack language is documented [here](https://github.com/VincentToups/emacs-utils/blob/master/with-stack.md) and the source code in
this repository will tell you anything else you might want to know.
Both have in-emacs documentation for most functions/macros.  So far
the language supports the following operands

	'b '0>backward-char ; back one
	'B '1>backward-char ; back n (top of stack)
	'f '0>forward-char  
	'F '1>forward-char
	'd 'delete-forward0
	'D 'delete-forward
	'k 'delete-backward0
	'K 'delete-backward
	'q 'microstack->quotation ; parses a string using microstack to a stack quotation
	'Q 'string->quotation ; treats a string as a stack word, pushes onto stack
	'! 'call ; the stack language call function
	'? 'if ; stack language if
	'+ '+ ; stack language plus
	'- '-
	't 't
	'_ 'nil
	'm '0>push-mark ; make the current point the mark
	'M '0>mark ; put the mark position on the stack
	'g '1>goto-char ; goto char on stack
	'x 'kill-current-region ;kill the region between point and mark
	'* '*
	'/ '/
	'N 'do-n-times ; do a quotation n times.  
	'L 'loop ; stack loop function
	's '1>search-forward ; search forward
	'S '1>search-forward-regexp ; search forward for a string
	'c 'concat ; concat strings
	'i 'insert ; insert a string

Obviously I'm going to add more operands as I use the library and determine what I wish to do.  
Hope someone out there finds this interesting!

	
