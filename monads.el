(require 'cl)
(require 'utils)
(require 'defn)

;; (setf monad-maybe
;; 	  (tbl!
;; 	   :m-return (lambda (x) (list 'Just x))
;; 	   :m-bind (lambda (v f)
;; 			   (if (= (car v) 'None) v
;; 				 (funcall f (cadr v))))))

(defn Just [x] 
  (list 'Just x))
(defn None [] (list 'None))
(defun None? (o)
  (and (listp o) (eq (car o) 'None)))
(defn MaybeVal [x]
  (if (eq (car x) 'None) (error "This should not happen, you tried to get the value of None")
	(cadr x)))

(defun Possibilities (&rest args)
  (cons 'Possibilities args))

(setf monad-possibilities 
	  (tbl! 
	   :m-return (lambda (x) (Possibilities x))
	   :m-bind (lambda (v f)
				 (apply #'concat (loop for possibility in (cdr v) 
									   collect (cdr (f v)))))))

(setf monad-maybe
	  (tbl!
	   :m-return (lambda (x) (Just x))
	   :m-bind (lambda (v f)
				 (if (eq (car v) 'None) v
				   (funcall f (MaybeVal v))))))

(setf monad-id 
	  (tbl! :m-return (lambda (x) x)
			:m-bind (lambda (v f) (funcall f v))))

(setf monad-state 
	  (tbl! 
	   :m-return (fn [x] (fn [s] (list x s)))
	   :m-bind (fn [mv f] 
				   (fn [s] 
					   (dlet [[val new-state] (funcall mv s)]
						 (funcall (funcall f val) new-state))))))

(setf monad-cont 
	  (tbl! 
	   :m-return (fn [v]
					 (fn [c]
						 (funcall c v)))
	   :m-bind 
	   (fn [mv mf]
		   (fn [c]
			   (funcall mv (fn [v]
							   (funcall (mf v) c)))))))

(defn fetch-state []
  (fn [state]
	  (list state state)))

(defn set-state [val]
  (fn [state]
	  (list val val)))

(defn fetch-state-alist [key]
  (fn [state]
	  (list (alist state key) state)))

(defn set-state-alist [key val]
  (fn [state]
	  (list val (alist>> state key val))))

(defmacro* defstatefun (name monad-forms &body body)
  (let ((state (gensym "state")))
	`(defun ,name (,state)
	   (funcall
		(domonad monad-state ,monad-forms ,@body)
		,state))))


(setf monad-seq 
	  (tbl! :m-return (lambda (x) (list x))
			:m-bind (lambda (v f) (apply #'append (mapcar f v)))))

(defun monad-set (predicate)
  (lexical-let ((lpred predicate))
	(tbl! :m-return (lambda (x) (list x))
		  :m-bind (lambda (v f) (unique (apply #'append (mapcar f v)) lpred)))))

(defn m-m-bind [monad v f]
  (funcall (tbl monad :m-bind) v f))

(defn m-m-return [monad v]
  (funcall (tbl monad :m-return) v))



(defmacro* with-monad (monad &body body)
  `(labels ((m-return (x) (m-m-return ,monad x))
			(m-bind (v f) (m-m-bind ,monad v f))
			(>>= (v f) (m-m-bind ,monad v f)))
	 ,@body))
(defmacro* with-monad-dyn (monad &body body)
  `(flet ((m-return (x) (m-m-return ,monad x))
		  (m-bind (v f) (m-m-bind ,monad v f))
		  (>>= (v f) (m-m-bind ,monad v f)))
	 ,@body))

(defn halt [x]
  (fn [c] x))

(defn yield [x]
  (fn [c]
	  (list x (fn []
		  (funcall c x)))))

(defn bounce [x]
  (fn [c]
	  (fn []
		  (funcall c x))))

(defn m-chain [steps]
  (foldl 
   (fn [step chain-expr]
	   (fn [v] (m-bind (funcall chain-expr v) step)))
   #'m-return
   steps))

(dont-do 
 (with-monad monad-seq
			 ($ (list 10 11 12) >>=  (lambda (x) (m-return (+ x 1)))))
 )

(defmacro* domonad-helper* (forms &body body)
  (cond 
   ((= 0 (length forms)) `(m-return (progn ,@body)))
   (t 
	(let ((form (car (coerce forms 'list)))
		  (val (cadr (coerce forms 'list)))
		  (rest-forms (coerce (cddr (coerce forms 'list)) 'vector)))
	  `(m-bind ,val (fn ,(vector form) (better-domonad-helper ,rest-forms ,@body)))))))

(defmacro* domonad-helper* (forms &body body)
  (cond 
   ((= 0 (length forms)) `(m-return (progn ,@body)))
   (t 
	(dlet_ [[form val & rest-forms] forms]
	  `(m-bind ,val (fn ,(vector form) (better-domonad-helper ,rest-forms ,@body)))))))

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
		   [testx (list 1 2) testy (m-return (+ 1 testx))]
		   testy)
 )


(defmacro* domonad-inner (m-bind-sym m-return-sym forms &body body)
  (cond ((= 0 (length forms)) `(funcall ,m-return-sym (progn ,@body)))
		((>= (length forms) 2)
		 (dlet [[form val & rest-forms] forms]
		   `(funcall ,m-bind-sym 
					 ,val
					 (fn ,(vector form)
						 (domonad-inner 
						  ,m-bind-sym 
						  ,m-return-sym 
						  ,rest-forms
						  ,@body)))))
		(t (error "domonad requires an even number of m-bind forms"))))


(defmacro* domonad (monad forms &body body)
  (cond
   ((oddp (length forms)) (error "domonad requires an even number of forms")) 
   ((= 0 (length forms)) `(progn ,@body))
   ((>= (length forms) 2)
	(dlet [monad-sym 
		   (gensym "monad")
		   m-bind-sym 
		   (gensym "m-bind")
		   m-return-sym 
		   (gensym "m-return")
		   [form val & rest-forms]
		   forms]
	  `(dlet [,monad-sym
			  ,monad
			  ,m-bind-sym
			  (tbl ,monad-sym :m-bind)
			  ,m-return-sym
			  (tbl ,monad-sym :m-return)]
		 (funcall ,m-bind-sym
										;(funcall ,m-return-sym ,val)
				  ,val
				  (fn ,(vector form)
					  (domonad-inner 
					   ,m-bind-sym 
					   ,m-return-sym 
					   ,rest-forms 
					   ,@body))))))))


;; (defn Just [x] 
;;   (cond ((numberp x) (list 'Just x))
;; 		((and (listp x) (eq (car x) 'None)) (None))))

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

 (setf monad-what 
	   (tbl! :m-return (fn [x] (fn [] x))
			 :m-bind (fn [v f]
						 (funcall f (funcall v)))))

 (defun incr (x) (+ x 1))

 (with-monad monad-what
			 (m-bind (m-return 10) #'incr))

 (setf side-effect 0)

 (with-monad monad-what
 (setf res (domonad monad-what 
					[x (fn [] (progn (setf side-effect 100) 10))
					   y (fn [] (list 3))
					   z (fn [] (car y))]
					z)))

  (setf r (funcall res))
  (funcall (funcall (funcall r)))

  (require 'monads)
  )

(provide 'monads)





