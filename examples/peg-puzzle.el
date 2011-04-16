(require 'monads)
(require 'utils)
(require 'recur)
(require 'cl)
(provide 'peg-puzzle)


(defstruct peg-game (board nil) (history nil))

(defun dip-board (g f)
  (make-peg-game :board (funcall f (peg-game-board g))
				 :history (peg-game-history g)))
(defun pick-board (g f)
  (funcall f (peg-game-board g)))

(defun dip-history (g f)
  (make-peg-game :board (peg-game-board g)
				 :history (funcall f (peg-game-history g))))
(defun pick-history (g f)
  (funcall f (peg-game-history g)))

(defun history-cons (trans game)
  (dip-history (par #'cons trans) game))

(defun fresh-board ()
  (alist>>
   0 '(0)
   1 '(0 1)
   2 '(0 1 2)
   3 '(0 1 2 3)
   4 '(0 1 2 3 4)))

(defun pos (x y)
  (cons x y))

(defmacro let-pos (peg-binders &rest body)
  (cond ((empty? peg-binders) `(progn ,@body))
		(t (let ((binder (car peg-binders))
				 (peg-sym (gensym "peg-")))
			 `(let* ((,peg-sym ,(cadr binder))
					 (,(car (car binder)) (car ,peg-sym))
					 (,(cadr (car binder)) (cdr ,peg-sym)))
				(let-pos ,(cdr peg-binders) ,@body))))))

(setq *directions* '(:nw :ne :e :w :sw :se))
(defun move1 (pos direction)
  (let-pos (((x y) pos))
		   (let* ((new-pos
				   (case direction
					 (:nw (pos (- x 1) (- y 1)))
					 (:ne (pos x (- y 1)))
					 (:e (pos (- x 1) y))
					 (:w (pos (+ x 1) y))
					 (:sw (pos x (+ y 1)))
					 (:se (pos (+ x 1) (+ y 1))))))
			 (if (on-board? new-pos)
				 new-pos
			   nil))))

(recur-defun* move-n (n pos dir)
  (if (= n 0) pos
	(let ((new-pos (move1 pos dir)))
	  (if new-pos
		  (recur (- n 1) (move1 pos dir) dir)
		nil))))

(defun on-board? (pos)
  (let-pos (((x y) pos))
		   (and (>= y 0)
				(<  y 5)
				(>= x 0)
				(<= x y))))

(defun peg-at-board? (board pos)
  (let-pos (((x y) pos))
		   (mlet* monad-maybe^i
				  ((row (alist board y))
				   (at? ($ x in row)))
				  at?)))

(defun remove-peg (board pos)
  (let-pos (((x y) pos))
		   (alist-conjugate board
							y
							(lambda (row)
							  (filter 
							   (f-not (par #'= x)) row)))))

(defun n-sort-cons (n n-list)
  (cond ((empty? n-list) (list n))
		((= n (car n-list)) n-list)
		(($ n < (car n-list)) (cons n n-list))
		(t (cons (car n-list) (n-sort-cons n (cdr n-list))))))

(defun add-peg (board pos)
  (let-pos (((x y) pos))
		   (alist-conjugate board
							y (pal #'n-sort-cons x))))


(defun generate-hop (board pos dir)
  (lexical-let ((pos pos))
	(mlet*_ monad-maybe^i 
			((over (move1 pos dir))
			 (target (move-n 2 pos dir))
			 (over-occupied? (peg-at-board? board over))
			 (target-empty? (not (peg-at-board? board target))))
			(lambda (board)
			  (let* ((board (remove-peg board over))
					 (board (remove-peg board pos))
					 (board (add-peg board target)))
				board)))))

(defun positions ()
  (mlet* monad-seq^i 
		 ((y '(0 1 2 3 4))
		  (x (range 0 (+ y 1))))
		 (pos x y)))

(setq *positions* 
	  (positions))


(defun generate-hops (board)
  (if (solved? board) (list board)
	(mlet* monad-seq^i 
		   ((pos *positions*)
			(dir *directions*)
			(new-board (let-if hop (generate-hop board pos dir) (m-return hop) nil)))
		   new-board)))

(recur-defun* strdup-n (n el &optional (acc ""))
  (cond ((= n 0) acc)
		(t (recur (- n 1) el (concat acc el)))))

(defun print-row (row-number data)
  (concat (strdup-n (- 4 row-number) " ")
		  (join (loop for i from 0 to row-number collect
					  (if ($ i in data #'=) "x" "o")) " ")))

(defun print-board (board)
  (concat "\n" (join (loop for i from 0 to 4 collect
						   (print-row i (alist board i))) "\n") "\n"))

(defun apply-hop-to-game (game hop)
  (make-peg-game :board 
				 (funcall hop (peg-game-board game))
				 :history (cons hop (peg-game-history game))))

(defun start-states ()
  (mlet* monad-seq^i 
		 ((pos (positions))
		  (games 
		   (make-peg-game 
			:board
			(remove-peg (fresh-board) pos)
			:history (list (par #'remove-peg pos)))))
		 games))

(defun count-pegs (board)
  (reduce 
   (lambda (ac el)
	 (+ ac (length (cadr el))))
   board
   :initial-value 0))

(defun game-count-pegs (game)
  (pick-board game #'count-pegs))

(defun solved? (board)
  (= 1 (count-pegs board)))

(defun full? (board)
  (= 15 (count-pegs board)))

(defun game-generate-hops (game)
  (mlet* monad-seq^i 
		 ((hop (generate-hops (peg-game-board game)))
		  (new-game (apply-hop-to-game game hop)))
		 new-game))

(defun game-remove-peg* (game)
  (if (pick-board game #'full?)
	  (start-states)
	(game-generate-hops game)))

(defun game-remove-peg (game pos)
  (dip-board game (par #'remove-peg pos) ))

(defun fresh-game ()
  (make-peg-game :board (fresh-board)
				 :history nil))

(defun solve-all-peg-games () 
  (with-monad monad-seq^i
			  (loop with games = (list (fresh-game)) 
					for i from 1 to 15 do
					(setq games ($ games >>= #'game-remove-peg*))
					finally (return games))))

(recur-defun* first-n (n lst &optional acc)
  (cond ((= n 0) (reverse acc))
		(recur (- n 1) (cdr lst) (cons (car lst) acc))))

(defun solve-peg-game (initial-condition)
  (let ((n-left (game-count-pegs initial-condition)))
	(with-monad monad-seq^i
				(loop with games = (list initial-condition) 
					  for i from 1 to n-left 
					  while (not (empty? games))
					  do
					  (print (format "peg %d, (length games) -> %d \n" i (length games)))
					  (setq games ($ games >>= #'game-remove-peg*))
					  finally (return games)))))


(defun game-board->string (game)
  (pick-board game 
			  #'print-board))









