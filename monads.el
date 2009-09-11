(require 'cl)
(require 'utils)
(require 'defn)

(setf monad-maybe
	  (tbl!
	   :result (lambda (x) (list 'Just x))
	   :bind (lambda (v f)
			   (if (= (car v) 'None) v
				 (funcall f (cadr v))))))
(setf monad-maybe
	  (tbl!
	   :result (lambda (x) (Just x))
	   :bind (lambda (v f)
			   (if (eq (car v) 'None) v
				 (funcall f (MaybeVal v))))))

(setf monad-id 
	  (tbl! :result (lambda (x) x)
			:bind (lambda (v f) (funcall f v))))

(setf monad-state 
	  (tbl! 
	   :result (fn [x] (fn [s] (list x s)))
	   :bind (fn [mv f] 
 				 (fn [s] 
					 (dlet [[val new-state] (funcall mv s)]
					   (funcall (funcall f val) new-state))))))
(setf monad-seq 
	  (tbl! :result (lambda (x) (list x))
			:bind (lambda (v f) (apply #'append (mapcar f v)))))

(defn m-bind [monad v f]
  (funcall (tbl monad :bind) v f))

(defn m-result [monad v]
  (funcall (tbl monad :result) v))


(defmacro* with-monad (monad &body body)
  `(labels ((result (x) (m-result ,monad x))
			(bind (v f) (m-bind ,monad v f))
			(>>= (v f) (m-bind ,monad v f)))
	 ,@body))
 


(dont-do 
 (with-monad monad-seq
			 ($ (list 10 11 12) >>=  (lambda (x) (result (+ x 1)))))
)

(defmacro* domonad-helper* (forms &body body)
  (cond 
   ((= 0 (length forms)) `(result (progn ,@body)))
   (t 
	(let ((form (car (coerce forms 'list)))
		  (val (cadr (coerce forms 'list)))
		  (rest-forms (coerce (cddr (coerce forms 'list)) 'vector)))
	  `(bind ,val (fn ,(vector form) (better-domonad-helper ,rest-forms ,@body)))))))

(defmacro* domonad-helper* (forms &body body)
  (cond 
   ((= 0 (length forms)) `(result (progn ,@body)))
   (t 
	(dlet_ [[form val & rest-forms] forms]
	  `(bind ,val (fn ,(vector form) (better-domonad-helper ,rest-forms ,@body)))))))

(defmacro* domonad* (monad forms &body body)
  (cond 
   ((oddp (length forms)) (error "domonad requires an even number of forms"))
   (t
	`(with-monad ,monad
				 (domonad-helper* ,forms ,@body)))))


(dont-do 
 (domonad monad-seq 
		  [testx (list 1 2) testy (list 1 2)]
		  (list testx testy))

 (domonad* monad-seq
		   [testx (list 1 2) testy (result (+ 1 testx))]
		   testy)
)


(defmacro* domonad-inner (bind-sym result-sym forms &body body)
  (cond ((= 0 (length forms)) `(funcall ,result-sym (progn ,@body)))
		((>= (length forms) 2)
		 (dlet [[form val & rest-forms] forms]
		   `(funcall ,bind-sym 
					 ,val
					 (fn ,(vector form)
						 (domonad-inner 
						  ,bind-sym 
						  ,result-sym 
						  ,rest-forms
						  ,@body)))))
		(t (error "domonad requires an even number of bind forms"))))


(defmacro* domonad (monad forms &body body)
  (cond
   ((oddp (length forms)) (error "domonad requires an even number of forms")) 
   ((= 0 (length forms)) `(progn ,@body))
   ((>= (length forms) 2)
	(dlet [monad-sym 
		   (gensym "monad")
		   bind-sym 
		   (gensym "bind")
		   result-sym 
		   (gensym "result")
		   [form val & rest-forms]
		   forms]
	  `(dlet [,monad-sym
			  ,monad
			  ,bind-sym
			  (tbl ,monad-sym :bind)
			  ,result-sym
			  (tbl ,monad-sym :result)]
		 (funcall ,bind-sym
										;(funcall ,result-sym ,val)
				  ,val
				  (fn ,(vector form)
					  (domonad-inner 
					   ,bind-sym 
					   ,result-sym 
					   ,rest-forms 
					   ,@body))))))))


(defn Just [x] 
  (cond ((numberp x) (list 'Just x))
		((and (listp x) (eq (car x) 'None)) (None))))
(defn Just [x] 
  (list 'Just x))
(defn None [] (list 'None))
(defn MaybeVal [x]
  (if (eq (car x) 'None) (error "This should not happen, you tried to get the value of None")
	(cadr x)))

(defn maybe+ [x y]
  (Just (+ x y)))

(defn maybe/ [x y]
  (if (= 0 y) (None)
	(Just (/ x y))))
(comment
 
 (domonad monad-id [x 10 y 11] (+ x y))
 (domonad monad-maybe [x (Just 20) k (maybe/ x 4) y (maybe+ k 1)] k)
 (domonad monad-maybe [x (Just 20) k (maybe/ x 0) y (maybe+ k 1)] k)
 (domonad monad-seq [x (list 1 2 3) y (list 4 5 6)] (list x y))

 (defn state-incr [state]
   (list state (+ 1 state)))

 (funcall (domonad monad-state
				   [a 
					#'state-incr 
					b 
					#'state-incr
					c
					#'state-incr]
				   (list a b c)) 0)

)

(provide 'monads)





