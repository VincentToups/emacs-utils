Microstack
----------
> Inspired by Vi's command mode but with an emacsy flavor.

Introduction
------------

Microstack is sort of like Vi's command mode, in that it turns a
series of single letter strokes and numerical (and string) arguments
into actions in the current buffer (or computations).  

On the other hand, its just a simple wrapper which lets you write a
subset of my other tool, [the emacs stack language](https://github.com/VincentToups/emacs-utils/blob/master/with-stack.md), in a highly concise
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

    [k]10N

You'll find emacs has deleted the last ten characters.

    "dogs and cats"s4D"bees"i

Will jump forward to the first location of "dogs and cats" after the
cursor, delete dogs, and replace it with bees.

Spaces are ignored except when they separate numbers, so all the above
can be rewritten with extra spaces, if you want to.

What the hell is going on here?
-------------------------------

Lets take a more complex example than the ones above and read it out
piece by piece, then discuss its stack language representation.

    [d][@"}"=]U

This piece of code reads "delete forward until you reach a }".  How do
I get that from the above?  `[d]` is a quotation, because it starts
with `[` and ends with `]`.  `d` means "delete forward".  So `[d]`
pushes a quotation which, when called, deletes one character forward.  

Immediately after this quotation is another, `[@"}"=]`, which pushes
the character under the cursor onto the stack (`@`), pushes "}", and
then pushes the result of testing whether they are equal (`=`).
Remember, this is just a quotation, so its just pushed onto the stack.
Neither quote is evaluated yet.

The next operand is `U`, which stands for `loop-until`, a stack word
which executes the lower quotation until the upper quotation pushes a
true onto the stack.  `Loop-unti` consumes the sentinal value on each
loop.  Putting all this together, we recover "delete forward until a
}".

In general, microstack takes a specially formatted string, parses it
into a series of single-character "operands," numbers or strings and
then passes that sequence through a translator which transforms
symbols into stack-language words using a dictionary.  Operands are
single characters, strings are like Lisp strings, numbers like lisp
numbers, and the only special syntax is `[]` for quotation.
Everything inside a `[]` is compiled into a stack-quotation using the
microstack compiler.

The idea is to provide the most useful/common stack language words for
text manipulation as single character operands, for brevity, while
allowing the full power of the emacs stack language. 

How can I learn more about this amazing invention??
---------------------------------------------------

The emacs stack language is documented [here](https://github.com/VincentToups/emacs-utils/blob/master/with-stack.md) and the source code in
this repository will tell you anything else you might want to know.
Both have in-emacs documentation for most functions/macros.  So far
the language supports the following operands

	'b 'backward ; move the point backward once
	'B '1>backward-char ; move the point backward n times, pop n from the stack
	'f 'forward ; move the point forward once
	'F '1>forward-char ; move the point forward n times, pop n from the stack
	'd 'delete-forward0 ; delete forward once
	'D 'delete-forward ; delete forward n times, pop n from the stack
	'k 'delete-backward0 ; delete backward once
	'K 'delete-backward ; delete backward n times, remove n from the stack
	'q 'microstack->quotation ; convert a STRING to a microstack compiled quotation, "..."q is eq to [...]
	'Q 'string->quotation ;push the stack word represented by string onto the stack to be called later
	'! 'call ; call a quotation/stack word
	'? 'if ; if 
	'+ '+ ; plus
	'- '- ; -
	't 't ; push t 
	'_ 'nil ; push nil
	'm '0>push-mark ; mark the current point as the mark
	'M '0>mark ; put the mark position on the stack
	'g '1>goto-char ; jump to a character number popped from the stack 
	'x 'kill-current-region ; kill the current region between the point and mark
	'* '* ; times
	'/ '/ ; divide
	'= '2>equal ; equals
	'N 'do-n-times ; do a quotation n times before stopping
	'L 'loop ; the loop word in all its general glory - execute a quotation until the top of the stack is true
	'{ '{{ ; start a list
	'} '}} ; end a list
	's '1>search-forward ; search forward for the string on the stack, which is popped
	'S '1>search-forward-regexp ; search forward for the regex on the stack, which is popped
	'c 'concat ; concat two strings
	(intern ",") 'print-stack ; print the stack
	(intern ":") 'dup ; dup
	(intern "$") 'swap ; swap the top two stack elements
	(intern "#") 'length ; pop object off the stack and push its length
	(intern "@") 'char-at-point->string ;push the current character onto the stack
	(intern ".") 'print ; print the top of the stack, pop it
	(intern "%") 'format ; lst format-string format; calls format with the string format-string and lst as rest args
	(intern "|") 'compose ; compose two quotations
	(intern "/") 'curry ; curry the value on the stack into the quotation below it.
	'U 'loop-until ; qt pred loop-until ; loop qt until pred is true
	'W 'loop-while ; qt pred loop-while ; loop qt while pred is true
	'i 'insert ; insert the top of the stack as text into the buffer


Obviously I'm going to add more operands as I use the library and determine what I wish to do.  
Hope someone out there finds this interesting!

How do I get it?
----------------

Pull this repository using git, add it to your emacs path, and `(require 'microstack).  You might get some warnings as the stack language compiles (it is sort of weird).
