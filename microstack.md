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

    4k"test"i

You will find that emacs has deleted the four characters ahead of your
position in the buffer, and then inserted the text "test".

If you enter instead

    [-1k]10N

You'll find emacs has deleted the last ten characters.  You might have instead just said 
directly -10k.

    "dogs and cats"s4k"bees"i

Will jump forward to the first location of "dogs and cats" after the
cursor, delete dogs, and replace it with bees.

Spaces are ignored except when they separate numbers, so all the above
can be rewritten with extra spaces, if you want to.

What the hell is going on here?
-------------------------------

Lets take a more complex example than the ones above and read it out
piece by piece, then discuss its stack language representation.

    [1k][@"}"=]U

This piece of code reads "delete forward until you reach a }".  How do
I get that from the above?  `[1k]` is a quotation, because it starts
with `[` and ends with `]`.  1 pushes 1 onto the stack `k` means
"kill".  So `[1k]` pushes a quotation which, when called, deletes one
character forward.

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
single characters, strings are like Lisp strings (except they can be
delimimted with single or double quotes in pairs), numbers like lisp
numbers, and the only special syntax is `[]` for quotation.
Everything inside a `[]` is compiled into a stack-quotation using the
microstack compiler.

The idea is to provide the most useful/common stack language words for
text manipulation as single character operands, for brevity, while
allowing the full power of the emacs stack language. 

Generic Motion and Deletion 
---------------------------

If you've been following this repository, you may have noticed a large
change in the stack words for motion.  Whereas before there was an
attempt to provide specific words for all sorts of motion and deletion
cases, I've rewritten this library (using my implementation of
[multimethods](https://github.com/VincentToups/emacs-utils/blob/master/multi-methods.md)) to provide generic motion and deletion words.  Instead
of a variety of words we now have only `m` for "move" and `k` for `kill`.

Both take their motion from the top of the stack.  If the stack holds
a number, then they simply move or delete characters, forward for
positive numbers and backwards for negative ones.  

If you want to delete or move by some other method, you must specify
this by decorating the number on the stack with descriptor words.  EG:

    4m

Moves forward 4 characters.

    4wm 

Moves forward 4 words (using foward-word).

    4pm

Moves four paragraphs.

    4em 

Four s-expressions.  Etc.  See the documentation below for all modifiers.

The modifiers also work with the kill word, so

    -4wk 

Deletes four words backward, including the current word.  Emacs has
lots of `forward-<something>` words, and there aren't really enough
keys to support them all, so you can call any motion type using the G 
word, which takes its motion from a string.

    -4"sentence"Gk

Kills the last four sentences, including the current one.


How can I learn more about this amazing invention??
---------------------------------------------------

The emacs stack language is documented [here](https://github.com/VincentToups/emacs-utils/blob/master/with-stack.md) and the source code in
this repository will tell you anything else you might want to know.
Both have in-emacs documentation for most functions/macros.  So far
the language supports the following operands

	'm 'move ; generic movement.  pops an item from the stack, then moves appropriately 
	'k 'kill ; generic move-and-kil, pops and item of the stack, marks, moves, and kill-region's
	'l 'line ; specify that a number indicates a number of lines
	'w 'word ; specify that a number indicates a number of words
	'y 'sym  ; specify that a number indicates a number of symbols
	'p 'paragraph ; specify that a number indicates a number of paragraphs
	'P 'page ; specify that a number indicates a number of pages
	'e 's-expression ; specify that a number indicates a number of s-expressions
	'G 'make-quantity-of ; take a string and a number and create a general quantity 4"sentence"G -> (:sentence 4)
	'q 'microstack->quotation ; convert a STRING to a microstack compiled quotation, "..."q is eq to [...]
	'Q 'string->quotation ;push the stack word represented by string onto the stack to be called later
	'! 'call ; call a quotation/stack word
	'? 'if ; if 
	'+ '+ ; plus
	'- '- ; -, note that - before a number without a space will be read as a negative number.  delimite - with spaces to indicate the operator.
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
	'o 'rot
	(intern ",") 'print-stack ; print the stack
	(intern ":") 'dup ; dup
	(intern "$") 'swap ; swap the top two stack elements
	(intern "#") 'length ; pop object off the stack and push its length
	(intern "@") 'char-at-point->string ;push the current character onto the stack
	(intern ".") 'print ; print the top of the stack, pop it
	(intern "%") 'format ; lst format-string format; calls format with the string format-string and lst as rest args
	(intern "|") 'compose ; compose two quotations
	(intern "^") 'curry ; curry the value on the stack into the quotation below it.
	'U 'loop-until ; qt pred loop-until ; loop qt until pred is true
	'u 'loop-until-char ; qt char loop-until-char; loop qt until char is beneath the cursor.
	'W 'loop-while ; qt pred loop-while ; loop qt while pred is true
	'i 'insert ; insert the top of the stack as text into the buffer



Obviously I'm going to add more operands as I use the library and determine what I wish to do.  
Hope someone out there finds this interesting!

How do I get it?
----------------

Pull this repository using git, add it to your emacs path, and `(require 'microstack).  You might get some warnings as the stack language compiles (it is sort of weird).
