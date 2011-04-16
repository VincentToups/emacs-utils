(require 'monads)
(require 'utils)
(require 'recur)




(defstruct peg-game (board nil) (history nil))
(defun dip-history (f game)
  (make-peg-game :board (peg-game-board game) :history 
				 (funcall f (peg-game-history game))))

(defun dip-board (f game)
  (make-peg-game :board 
				 (funcall f (peg-game-board game))
				 :history (peg-game-history game)))

(defun bi-game (f-board f-history game)
  (make-peg-game 
   :board (funcall f-board (peg-game-board game))
   :history (funcall f-history (peg-game-history game))))

(defun history-cons (game hist-element)
  (dip-history (pal #'cons hist-element) game))

(defmacro* defgame-board-function (name args &rest body)
  (with-gensyms (game)
				`(progn 
				   (defun ,name ,args ,@body)
				   (defun ,(s-cat 'game name) (,game ,@(cdr args))
					 (dip-board (funcall #'par #',name ,@(cdr args)) ,game)))))


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

(defun pos< (p1 p2)
  (let-pos (((x1 y1) p1)
			((x2 y2) p2))
		   (cond ((< x1 x2) t)
				 ((> x1 x2) nil)
				 ((= x1 x2) (< y1 y2)))))

(defun pos= (p1 p2) 
  (and (= (car p1) (car p2))
	   (= (cdr p1) (cdr p2))))

(defun pos-cons (pos lst)
  (cond 
   ((empty? lst) (list pos))
   ((pos= (car lst) pos) lst)
   ((pos< pos (car lst)) (cons pos lst))
   (t (cons (car lst) (pos-cons pos (cdr lst))))))

(setq *min* 1)
(setq *max* 3) ; the board is square

(defun legal-position? (pos)
  (and (between-inc *min* *max* (car pos))
	   (between-inc *min* *max* (cdr pos))))

(recur-defun* peg-at? (board pos)
  (cond ((empty? board) nil)
		((pos= (car board) pos) t)
		(t (recur (cdr board) pos))))

(defgame-board-function add-peg (board pos)
  (if (not (legal-position? pos)) 
	  (error "pos %S not legal on a %s x %s board" pos *max* *max*))
  (if (peg-at? board pos)
	  (error "peg already at %S in board %S." pos board))
  (pos-cons pos board))

(defgame-board-function remove-peg (board pos)
  (cond ((empty? board) nil)
		((pos= (car board) pos) (cdr board))
		(t (cons (car board) (remove-peg (cdr board) pos)))))

(defgame-board-function full-board ()
  (foldl 
   (lambda (x ac)
	 (foldl 
	  (lambda (y ac)
		(add-peg ac (pos x y)))
	  ac
	  '(3 2 1)))
   nil
   '(3 2 1)))

(defgame-board-function board-solved? (board)
  (= (length board) 1))

(defgame-board-function row->string (board row)
  (join (mapcar 
		 (lambda (c)
		   (if (peg-at? board (pos c row)) "x" "o"))
		 (range *min* (+ 1 *max*))) " "))



(defgame-board-function board->string (board)
  (concat (format "\n") (join (mapcar (pal #'row->string board) (range *min* (+ 1 *max*))) (format "\n"))
		  (format "\n")))



(defun pos-metric (p1 p2)
  (let-pos (((x1 y1) p1)
			((x2 y2) p2))
		   (+ (abs (- x1 x2))
			  (abs (- y1 y2)))))


(defun legal-direction? (direction)
  (or (eq direction :north)
	  (eq direction :south)
	  (eq direction :east)
	  (eq direction :west)))

(defun* move-by (pos direction amount)
  (let-pos 
   (((x y) pos))
   (let
	   ((dest 
		 (case direction
		   (:north (pos x (- y amount)))
		   (:south (pos x (+ y amount)))
		   (:east (pos (- x amount) y))
		   (:west (pos (+ x amount) y)))))
	 (if (legal-position? dest) dest nil))))

(defgame-board-function legal-hop? (board from direction)
  (mlet* monad-maybe^i 
		 ((over (move-by from direction 1))
		  (over-occupied (peg-at? board over))
		  (target (move-by from direction 2))
		  (target-unoccupied 
		   (not (peg-at? board target))))
		 (lexical-let ((from from))
		   (lambda (board)
			 (let* ((board (remove-peg board over))
					(board (remove-peg board from))
					(board (add-peg board target)))
			   board)))))

(defgame-board-function generate-legal-hops (board)
  (mlet* monad-seq^i 
		 ((start-position board)
		  (direction '(:north :south :east :west))
		  (hops 
		   (let-if hop (legal-hop? board start-position direction) 
				   (m-return hop) 
				   nil)))
		 hops))


(defvar *start* (remove-peg (full-board) (pos 1 2)))

(generate-legal-hops *start*)

(defun game-solved? (game)
  (board-solved? (peg-game-board game)))

(defun step-game (game)
  (if (game-solved? game) (list game)
	(let ((hops (generate-legal-hops (peg-game-board game))))
	  (loop for hop in hops collect 
			(dip-board hop (history-cons game hop))))))

(defvar *fresh-game* (make-peg-game :board *start* :history nil))

(defun pass-print (o)
  (print o)
  o)



(generate-legal-hops (full-board))

(defun remove-any-peg (game)
  (mlet*_ monad-seq^i 
		  ((peg (peg-game-board game))
		   (new-game 
			(game-remove-peg-historically game peg)))
		  (m-return new-game)))

(lex-defun game-remove-peg-historically (game peg)
  (history-cons (dip-board (par #'remove-peg peg) game)
				(lexical-let ((peg peg))
				  (lambda (game)
					(game-remove-peg-historically game peg)))))

(defun fresh-game ()
  (make-peg-game :board (full-board) :history nil))

(remove-any-peg (fresh-game))

(mlet*_ monad-seq^i
		((x (range 10))
		 (y (range 11))
		 (z (m-return (+ x y))))
		z)


(mlet*_ monad-seq^i
		((game (remove-any-peg (fresh-game)))
		 (game2 (step-game game))
		 (game3 (step-game game2))
		 (game4 (step-game game3))
		 (game5 (step-game game4)))
		(with-current-buffer "*scratch*"
		  (insert (board->string (peg-game-board game5)))))


(peg-game-board (car (remove-any-peg (fresh-game))))

(unique-map-cat #'step-game (step-game ))

(fix (m-lift 1 #'step-game) (list (make-peg-game :board *start* :history nil)))

(board->string (peg-game-board (car (step-game (car (step-game *fresh-game*))))))

(board->string (peg-game-board *fresh-game*))

(defun unique-map-cat (f seq)
  (unique (mapcat f seq) #'equal))

(unique-map-cat #'step-game (unique-map-cat #'step-game (unique-map-cat #'step-game (unique-map-cat #'step-game (unique-map-cat #'step-game (list (make-peg-game :board *start* :history nil)))))))

(fix (pal #'unique-map-cat #'step-game) (list (make-peg-game :board *start* :history nil)))
				 


(peg-at? *start* (move-by (pos 1 1) :north 1))


(defun hop (from direction)


(defun print-board (board)
  (loop for i in (range 1 5) do
		(loop for