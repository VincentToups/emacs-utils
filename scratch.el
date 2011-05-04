(require 'with-stack)
(require 'stack-words)


(||| 
 word: seq-return 1>list end:
 word: seq-bind ; (list func)
 map 'append swap 2>apply end:)

(|||p { {{ 1 2 3 4 }} 
	  { { 1 + } { 1 - } bi 2>list }
	  { + 1>list }

      '(list-bind)
	  do-monadically )

{{ 1 2 3 4 }} q1 bind
with
q1 { { { 1 + } { 1 - } bi 2>list } q2 bind }
with 
q2 { + 1>list }
      

	  (bind '(1 2 3 4) 
			(lambda (x1) 
			  (bind (list (+ x1 1) (- x1 1))
                (lambda (x2) 
                       (m-return (+ x1 x2))))))

(|||p '(1 2 3) '('(2 +) '(2 -) bi 2>list) seq-bind
'('(1 +) '(1 -) bi 2>list) seq-bind)

(|||p '(2 +) )

(|||p '(1 2 3 4) 4 '(+) curry map)

(|||p '(list-bind) dup '(call) curry)

(word: state-return ;( object -- qtn( stack -- {/ object stack  /} ))
	   2>list)

(||| 
word: Just? dup listp '(car 'Just eq) '(nil) if end:
word: Just '(Just) swap suffix end:
word: Just-val 1>cadr end:
word: Error '(Error) swap suffix end:
word: Error? dup listp '(car 'Error eq) '(nil) if end:
word: error-return Just end:
word: error-bind ;( v f -- mr )
      swap dup Error? '() '(Just-val swap call) if end:
word: safe-div dup 0 = '(drop "Divide by zero error." Error) '( / Just) if end:
word: implicit-error-bind ;( v f -- mr )
      swap dup Error? '() '(drop swap print-stack call) if end:
)

(||| 30 Just '(3 safe-div) error-bind
     '( 10 + Just ) error-bind)

(||| word: im-safe-div dup 0 = '(drop "Divide by zero." Error) '( / ) if end:)

(||| 30 '(0 im-safe-div) implicit-error-bind)

(|||
 word: extract-monad   '(car) '(cadr) bi end:
 word: cons-twice cons cons end:
 word: fold-bind-into ;( seq bind-op -- seq )
 '( swap cons _ swap cons ) fry 
 nil swap foldl reverse end:

 word: fold-bind-into&quotify-atoms ;( seq bind-op -- seq )
 '( quote-list-if-not-quote-list swap  cons _ swap cons ) fry 
 nil swap foldl reverse end:

 word: monadically ;( initial qt-seq bind -- monadic-value )
 fold-bind-into 
 call end:

 word: monadically* ;( initial qt&atom-seq bind -- monadic-value )
 thread-quote call end:
 
 word: tag ;( value tag -- tagged-value )
  swap 2>list end:
 word: tagged-with? ;( value tag -- boolean )
  swap dup listp '(car eq) '(drop drop nil) if end:
 word: untag ;(tagged-value -- value )
  cadr end:

  word: Branches 'Branches tag end:
  word: Branches? 'Branches tagged-with? end:

  word: implicit-sequence-bind ;( v f -- mr )
  swap dup Branches? print-stack
  '(untag swap seq-bind Branches)
  '(1>list swap seq-bind Branches) if
  end:
 

)

(word: make-quotation '() curry 'quote swap 2>list)
(word: cons-twice ;( item item lst )
 '(cons) curry dip swons)
(word: 2prefix ;( a b lst -- (a b @lst)
  prefix prefix )

(|||p 'a 'b '(c) 2prefix)

(word: swons ;( list item ) 
  swap cons )
(word: swons-twice ;( list item item )
  
(word: thread-quote-step ;( thread -- quot )
  ;( lst item -- lst )
  '( make-quotation swons _ swons ) fry)
(word: thread-quote ;( lst thread -- threaded )
  thread-quote-step nil swap foldl reverse )

(|||p 10 'x tag 'y tagged-with?)

(|||p 0 'Branches tagged-with?)

(|||p 10 1>list Branches '( '(14 12 2>list Branches) + ) implicit-sequence-bind monadically* )
(|||p 0 '(3 +) ''implicit-sequence-bind fold-bind-into&quotify-atoms)

(|||p 0 '( '(1 +) '(1 -) bi 2>list Branches ) ''implicit-sequence-bind monadically*)
      
(word: quote-list-if-not-quote-list
  dup listp '() '(1>list 'quote swap 2>list) if)

(|||p 'x list-if-not-list)

(|||p 4 '( 0 safe-div ) ''implicit-error-bind monadically* )

(||| 10 error-return Just?)
)

(|||p '(a b c '(d)) ''x fold-bind-into&quotify-atoms)

(||| 
 word: mark-for-reification 
 "unreification-marker" 1>gensym dup >r 
 end: 
 word: state-bind ;( mv f -- qtn( stack -- {/ result stack /} ))
       swap
       '( mark-for-reification 
          swap push-list 
          _
   

