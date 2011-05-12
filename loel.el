(provide 'loel)
(require 'cl)
(require 'recur)
(require 'monads)
(require 'streams)
(require 'utils)

(defvar *var-tag-table* 
  (make-hash-table :weakness 'key-and-value)
  "var-tag-table is a weak table which helps identifty loel variables.")

(defun* new-var-tag (var-sym)
  (let-if tag (gethash var-sym *var-tag-table*) tag
		  (let ((tag (gensym "")))
			(puthash var-sym tag *var-tag-table*)
			tag)))

(defun tag-in-table? (sym tag)
  (eq (gethash sym *var-tag-table* nil)
	  tag))

(defun f-var (sym)
  (vector '--var (new-var-tag sym) sym))

(defun var-tag (var)
  (elt var 1))

(defun var? (o)
  (and (vectorp o)
	   (= (length o) 3)
	   (eq (elt o 0) '--var)
	   (tag-in-table? (var-sym o) (var-tag o))))

(defun var-sym (var)
  (elt var 2))

(defvar *symbol-counter* 0)
(defun new-symbol ()
  (prog1 (internf "_%d"  *symbol-counter*)
	(setq *symbol-counter* (+ 1 *symbol-counter*))))

(defmacro* var (&optional (symbol (new-symbol)))
  (if (not (symbolp symbol))
	  (error "Var must be initialized with a symbol.")
	`(f-var ',symbol)))


(defun choice (a &optional f)
  (stream a f))

(defvar choice-zero nil)

(defun choice-unit (a)
  (stream a))


(defmacro goal (arg &rest body) `(lex-lambda (,(car arg)) ,@body))
(defmacro defgoal (name arg &rest body) 
  (declare (indent defun))
  `(lex-defun ,name (,(car arg))
	 ,@body))

(defgoal s (v) (stream v))
(defgoal fail (u) choice-zero)

(lex-defun =%= (v w)
  (goal (s)
		(let ((ucation
			   (loel-unify v w s)))
		  (cond 
		   (ucation (s ucation))
		   (t (fail s))))))

(defmacro fresh (vars &rest body)
  (with-gensyms 
   (u)
   `(goal (,u)
		  (lexical-let
			  ,(loop for var in vars collect `(,var (var ,var)))
			(funcall (loel-all ,@body) ,u)))))

(defvar nil-sub nil)

(recur-defun* walk-sub (v sub)
  (let-if asc (alist sub v) 
		  (if (var? asc)
			  (recur asc sub)
			asc)
		  v))

(defun* walk-sub* (v sub)
  (let ((v (walk-sub v sub)))
	(cond 
	 ((var? v) v)
	 ((listp v)
	  (cons 
	   (walk-sub* (car v) sub)
	   (walk-sub* (cdr v) sub)))
	 (t
	  v))))

(defun ext-sub (x v s)
  (alist>> s x v))

(defun loel-unify (v w s)
  (let ((v (walk-sub v s))
		(w (walk-sub w s)))
	(cond
	 ((eq v w) s)
	 ((var? v) (ext-sub v w s))
	 ((var? w) (ext-sub w v s))
	 ((and 
	   (listp v)
	   (listp w))
	  (let-if unified
			  (loel-unify (car v) (car w) s)
			  (loel-unify (cdr v) (cdr w) unified)
			  nil))
	 ((equal v w) s)
	 (t nil))))

(defun reify-name (n)
  (internf "_%d" n))

(defun reify-s (v &optional s)
  (let ((v (walk-sub v s)))
	(cond 
	 ((var? v) (ext-sub v (reify-name (length s)) s))
	 ((listp v)
	  (cons (reify-s (car v))
			(reify-s (cdr v))))
	 (t
	  s))))

(defun reify (v s)
  (walk-sub* v (reify-s v)))



(defmacro* loel-run (n varlst &body body)
  (with-gensyms 
   (nhat varhat shat)
   (db-print (list n varlst))
   `(lexical-let ((,nhat ,n)
				  (,(car varlst) (var ,(car varlst))))
	  (if (and 
		   (not (nil? ,nhat))
		   (> ,n 0))
		  (map-inf ,nhat 
				   (lambda (,shat)
					 (reify (walk-sub* ,(car varlst) ,shat) ,shat))
				   (funcall (loel-all ,@body) nil-sub))
		nil))))

(defmacro cond-aux (ifer &rest other-args)
  (dlet_ [[head & tail] other-args
          [h-head & h-tail] head ]
	(cond 
	 ((length= other-args 0) 'fail)
	 ((and 
	   (eq h-head 'else)
	   (length= tail 0))
	  `(loel-all ,@h-tail))
	 ((and
	   (not (eq h-head 'else))
	   (length= tail 0))
	  `(loel-all ,@head))
	 ((and
	   (length> head 0))
	  `(cond-aux ,ifer 
				 (loel-all ,@head)
				 (cond-aux ,ifer ,@tail))))))

(defmacro all-aux (bnd &rest other-args)
  (dlet_ [[g & gs] other-args]
	(cond ((empty? other-args) #'s)
		  ((empty? gs) g)
		  (t 
		   (with-gensyms 
 			(g-hat u u-inner)
			`(lexical-let ((,g-hat ,g))
			   (goal (,u)
					 (,bnd (funcall ,g-hat ,u)
						   (goal (,u-inner)
								 (funcall (all-aux ,bnd ,@gs) ,u-inner))))))))))

(defmacro loel-all (&rest gs)
  `(all-aux stream-bind ,@gs))

;; (defmacro loel-all (&rest gs)
;;   (with-gensyms (u)
;; 				`(goal (,u)
;; 					   (lexical-mlet< monad-stream
;; 									  ,(loop for g in gs collect
;; 											 `(,u (funcall ,g ,u)))
;; 									  ,u))))


(defmacro loel-all^i (&rest gs)
  `(all-aux stream-bind^i ,@gs))

(defmacro if^e (g0 g1 g2)
  (with-gensyms 
   (u)
   `(goal (,u)
		  (stream-cat 
		   (funcall (all ,g0 ,g1) ,u)
		   (later (funcall ,g2 ,u :with (,u)))))))

(defmacro if^i (g0 g1 g2)
  (with-gensyms 
   (u)
   `(goal (,u)
		  (stream-interleave  
		   (funcall (all ,g0 ,g1) ,u)
		   (later (funcall ,g2 ,u :with (,u)))))))

(defmacro if^a (g0 g1 g2)
  (with-gensyms 
   (u u-inf g-hat u-head f)
   `(goal (,u) 
		  (lexical-let ((,u-inf (funcall ,g0 ,u))
						(,g-hat ,g1))
			(stream-case ,u-inf 
						 (funcall ,g2 ,u)
						 ((,u-head) (funcall ,g-hat ,u-head))
						 ((,u-head ,f) 
						  (stream-bind ,u-inf ,g-hat)))))))

(defmacro if^u (g0 g1 g2)
  (with-gensyms 
   (u u-inf u-head f g-hat)
   `(goal (,u)
		  (lexical-let ((,u-inf (funcall ,g0 ,u))
						(,g-hat ,g1))
			((funcall ,g2 ,u))
			((,u-head) (funcall ,g-hat ,u-head))
			((,u-head ,f)
			 (funcall ,g-hat ,u-head))))))



(defmacro cond^e (&rest cs)
  `(cond-aux if^e ,@cs))				

(defmacro cond^i (&rest cs)
  `(cond-aux if^i ,@cs))

(defmacro cond^a (&rest cs)
  `(cond-aux if^a ,@cs))

(defmacro cond^u (&rest cs)
  `(cond^u if^u ,@cs))

(dont-do
 (loel-run 1 (x) 
		   (fresh (y)
				  (=%= #'s y)
				  y))

 (loel-unify (var x) (var y) nil)		   

 (funcall (loel-all 
		   (=%= (var x) #'s)) nil)

 (funcall (=%= (var v) (var u)) nil)

 (cl-prettyexpand '(fresh (x) 
						  (=%= x #'s)))







)