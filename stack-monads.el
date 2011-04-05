(require 'with-stack)
(require 'stack-words)
(|||p 1 2 3 stack)
(|||p 1 2 3 4 stack '(drop-all) dip)
(|||p '{ 'a 'b 'c '{ 'd '} 'e '} stack '(drop-all) dip reverse)



(||| word: monad{ 
     stack '(drop-all) dip reverse cdr
     '{monad swap cons
     '{monad 'monad} split-by-match end:)
  
(|||p '({monad a b c d e monad} cats dogs raining) push-list monad{)
