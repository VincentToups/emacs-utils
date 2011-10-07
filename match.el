(require 'monads)
(require 'utils)
(require 'recur)

(defstruct pdoublet retval rest)

(defun pdoublet (retval rest) 
  (make-pdoublet :retval retval :rest rest))

(defvar else t)
(defvar otherwise t)

(lex-defun promote-parser (p)
  (cond 
   ((funcall (f-or #'numberp #'stringp #'symbolp #'keywordp) p)
	(lex-lambda (input)
				(if (empty? input) input
				  (let ((first (car input)))
					(if (equal p first) (list (pdoublet p (cdr input)))
					  nil)))))
   (else p)))

(lex-defun promote-parser<p> (p)
  (if (functionp p) p
	(lambda (v) (promote-parser p))))

(defun these (&rest rest)
  (lexical-let ((n (length rest))
				(these these)
				(rest rest))
	(lambda (input)
	  (let-seq (match-these leftover)
			   (split-after-n input n)
			   (if (equal match-these rest)
				   (list (pdoublet match-these leftover))
				 nil)))))

(defun parse-list-bind (parser parser<p>)
  (lexical-let ((parser (promote-parser parser))
				(parser<p> (promote-parser<p> parser<p>)))
	(lex-lambda (input)
				(if (empty? input) nil
				  (recur-let 
				   ((prl (funcall parser input))
					(acc '()))
				   (if (empty? prl) acc
					 (recur (cdr prl)
							(append acc  
									(let* ((pdoub (car prl))
										   (retval (pdoublet-retval pdoub))
										   (rest (pdoublet-rest pdoub))
										   (newp (promote-parser (funcall parser<p> retval))))
									  (funcall newp rest))))))))))

(lex-defun parse-list-return (item)
  (lex-lambda (input)
			  (list (pdoublet item input))))

(defun parse-list-plus (p1 p2)
  (lexical-let ((p1 (promote-parser p1))
				(p2 (promote-parser p2)))
	(lexical-mlet parse-list-monad
				  ((r1 p1)
				   (r2 p2))
				  (m-return r2))))

(lex-defun parse-list-zero (input)
  nil)

(setq parse-list-monad 
	  (tbl!
	   :m-return #'parse-list-return
	   :m-bind #'parse-list-bind
	   :m-plus #'parse-list-plus
	   :m-zero #'parse-list-zero))

(funcall (lexical-mlet parse-list-monad 
					   ((x 'x)
						(y 'y))
					   (m-return (list x y))) (list 'x 'y))


(lex-defun @or2 (p1 p2)
  (lambda (input)
	(when input
	  (let ((r1 (funcall p1 input)))
		(if r1 r1
		  (funcall p2 input))))))

(lex-defun @or (&rest ps)
  (reduce #'@or2 ps))

(lex-defun @and (&rest ps)
  (reduce #'parse-list-plus ps))

(lex-defun @satisfies (f)
  (lambda (input)
	(when input
	  (let ((first (car input))
			(rest (cdr input)))
		(if (funcall f first)
			(list (pdoublet f rest))
		  nil)))))

(defun @list-of (p)
  (lexical-mlet parse-list-monad 
				((r p))
				(m-return (list r))))

(defun zero-or-more-dealer (dblt p)
  (let ((input (pdoublet-rest dblt))
		(lst (pdoublet-retval dblt)))
	(let-if rs (funcall p input)
			(list :continued 
				  (mapcar 
				   (lambda (dblt)
					 (let* ((prv (pdoublet-retval dblt))
							(cont (pdoublet-rest dblt)))
					   (pdoublet (cons prv lst) cont))) rs))
			(list :terminated 
				  (pdoublet (reverse lst) input)))))

(defun @zero-or-more (p)
  (lexical-let ((p (promote-parser p)))
	(lambda (input)
	  (when input
		(recur-let 
		 ((rs 
		   (funcall (@list-of p) input))
		  (terminals '()))
		 (let* ((asc
				 (mapcar/deal (par #'zero-or-more-dealer p) rs))
				(cont (reduce #'append (alist asc :continued)))
				(term (alist asc :terminated)))
		   (if (empty? cont)
			   (append terminals term)
			 (recur cont
					(append terminals term)))))))))
		 
(defun @one-or-more (p)
  (lexical-let ((p (promote-parser p)))
	(lexical-mlet parse-list-monad
				  ((one p)
				   (more (@zero-or-more p)))
				  (m-return (cons one more)))))


(funcall (@zero-or-more 'x) '(x x x ))

(funcall (promote-parser 'x) '(x x x x))
