(require 'with-stack)
(require 'stack-words)


(||| 
word: seq-return 1>list end:
word: seq-bind ; (list func)
map 'append swap 2>apply end:)

(|||p '(1 2 3) '('(2 +) '(2 -) bi 2>list) seq-bind
'('(1 +) '(1 -) bi 2>list) seq-bind)


