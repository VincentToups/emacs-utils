(provide 'loel)
(require 'cl)
(require 'recur)
(require 'monads)
(require 'streams)
(require 'utils)

(defun choice (a &optional f)
  (stream a f))

(defvar choice-zero nil)

(defun choice-unit (a)
  (stream a))


(defmacro goal (arg ,&rest body) `(lex-lambda (,(car arg)) ,@body))
(defmacro defgoal (name arg &rest body) 
  (declare (indent defun))
  `(lex-defun ,name (,(car arg))
	 ,@body))

(defgoal s (v) (stream v))
(defgoal fail (s) choice-zero)

(lex-defun =%= (v w)
  (goal (s)
		(let ((ucation
			   (unify v w s)))
		  (cond 
		   (ucation (funcall s ucation))
		   (t (funcall fail s))))))

(defmacro fresh (vars &rest body)
  (with-gensyms 
   (s)
   `(goal (,s)
		  (lexical-let
			  ,(loop for var in vars collect `(,var (var ,var)))
			(funcall (loel-all ,@body) ,s)))))

(defmacro loel-run (n varlst &body body)
  (with-gensyms 
   (nhat varhat shat)
   `(lexical-let ((,nhat ,n)
				  (,varhat (var ',(car varlst))))
	  (if (and 
		   (not (nil? ,nhat))
		   (> n 0))
		  (map-inf ,nhat 
				   (lex-lambda (,shat)
							   (reify (walk* ,varhat ,shat)))
				   (funcall (all ,@body) empty-s))
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
 			(g-hat s s-inner)
			`(lexical-let ((,ghat ,g))
			   (goal (,s)
					 (,bnd (funcall ,ghat ,s))
					 (goal (,s-inner)
						   (funcall (all-aux ,bnd ,@gs) ,s-inner)))))))))


	   
