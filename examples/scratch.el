(push "/home/toups/elisp/utils/examples" load-path)
(require 'peg-puzzle-streams)



(setq centro-c
	  (game-remove-peg (fresh-game) '(1 . 2)))
(setq almost-solved 
	  (make-peg-game 
	   :board (alist>> 2 '(1 2))
	   :history nil))


(game-board->string almost-solved)"
    o
   o o
  o x x
 o o o o
o o o o o
"
(game-board->string (car (take-n (game-generate-hops almost-solved) 10)))"
    o
   o o
  x o o
 o o o o
o o o o o
"

(setq novel-solutions 
	  (let* ((init-board (peg-game-board (scar solutions)))
			 (predicate 
			  (lexical-let ((init-board init-board))
				(lambda (g)
				  (not (equal (peg-game-board g) init-board))))))
		(remove-until solutions predicate)))

(tick-tock
 (setq solutions
	   (solve-peg-game centro-c)))

(loop for s in (take-n solutions 100) do
	  (insert (game-board->string s)))

