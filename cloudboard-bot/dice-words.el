(require 'with-stack)
(require 'stack-words)
(require 'cl)

(|||
 word: dice ;( n-dice sides -- dice-object )
    2>list 'dice: swap 2>cons '(sided dice) 2>append end:)

(|||
 word: coin{s} 2 dice end:
 word: d2 2  dice end:
 word: d3 3  dice end:
 word: d4 4  dice end:
 word: d5 5  dice end:
 word: d6 6  dice end:
 word: d7 7  dice end:
 word: d8 8  dice end:
 word: d9 9  dice end:
 word: d10 10  dice end:
 word: d12 12  dice end:
 word: d20 20  dice end:
 word: d40 40  dice end:
 word: d50 50  dice end:
 word: d100 100  dice end:)



(univalent-stack-words listp stringp numberp not empty? error)



(||| 
 word: dice? ;( dice-object? -- boolean )
 dup listp '(car 'dice: eq) '(nil) if end:)

(|||
 word: suffix 2>suffix end: 
 word: dice-n&sides ;( dice-object -- n sides)
 '(1 2) 2>elts '(car) '(cadr) bi end:
 word: dice-sides&n ;( dice-object -- sides n)
 dice-n&sides swap end:)

(|||
 word: roll-die ;( n-sides -- roll )
 1>random* 1 + end:
 word: decr-dice-count ; ( sides n acc -- sides n-1 acc )
 '(1 -) dip end:
 word: get-dice-roll ; ( sides n acc -- sides n-1 acc roll )
 '(dup roll-die) 2dip rot end: 
 word: rolling-done? ; ( sides n acc -- sides n acc bool)
 '(dup 0 = not) dip swap end:  

 word: roll-dice-unsafe dice-sides&n 0 
 '( decr-dice-count 
	get-dice-roll 
	+ 
	rolling-done?  ) loop 
	'(drop drop) dip
	end:
	word: roll-dice-verbose ;( dice -- roll-summary )
	dice-sides&n nil
	'( decr-dice-count 
	   get-dice-roll 
	   swap cons 
	   rolling-done? ) loop 
	   '(drop drop) dip 
	   dup '(+) reduce :sum
	   swap 2>list append
	   end:
)




(||| 
  word: roll-dice ;( dice -- roll )
        dup dice? '(roll-dice-unsafe) '("Tried to roll a non dice object" error)
        if end:
  word: rl ;( dice -- roll )
        roll-dice end:
  word: roll-dice-collection ;( dice-collection -- roll )
        0
        '( '(tail&head) dip swap print-stack
            roll-dice + print-stack
           '(dup empty? not) dip swap ) loop
        '(drop) dip
        end:
  word: rlc ;( dice-collection -- roll )
        roll-dice-collection 
        end:)
(|||p 2 d6 roll-dice)

(|||p {{ 1 d6 2 d3 }} roll-dice-collection)
(||| {{ 1 1 3 + }} )
  
(||| lex-let x 10 :> 3 x + )
'(3 x +)
'(3 


(|||p
 10 1 50 rolling-done?)


