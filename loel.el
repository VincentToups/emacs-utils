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

(defgoal s% (s) (stream s))
(defgoal u% (s) choice-zero)

(lex-defun =%= (v w)
  (goal (s)
		(let ((ucation
			   (unify v w s)))
		  (cond 
		   (ucation (funcall s% ucation))
		   (t (funcall u% s))))))

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

