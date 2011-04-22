(require 'with-stack)
(require 'stack-words)


(word: flip 1.0 1>random* .5 < '(:heads) '(:tails) if)
(word: -toss ;( n - results )
  nil { flip swons { 1 - dup 0 = not } dip swap } loop )
(word: heads-count ;( flips - count )
  { :heads eq } filter length 'heads 2>list )

(word: tails-count ;( flips - count )
  { :tails eq } filter length 'tails 2>list )

(word: toss ; ( n - toss-result )
  -toss dup { heads-count } { tails-count } bi 2>list swap 2>list 1>flatten-once )

(word: botch-at? ; ( toss-result n - t/false )
  1 + { cdr cdr } dip elt :tails eq { :BOTCH } { :PASS } if )

(word: +botch-at? ; ( toss-result n - t/false )
  { dup } dip
  1 + { cdr cdr } dip elt :tails eq { :BOTCH } { :PASS } if )