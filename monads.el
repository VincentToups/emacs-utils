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

(defvar monad-maybe
  (tbl!
   :m-zero (None)
   :m-return (lambda (x) (Just x))
   :m-bind (lambda (v f)
			 (if (eq (car v) 'None) v
			   (funcall f (MaybeVal v)))))
  "The MAYBE monad.  See Just, None, None?, and MaybeVal.")

(defvar monad-id 
  (tbl! :m-return (lambda (x) x)
		:m-bind (lambda (v f) (funcall f v)))
  "The identity monad - you know, for kids.")

(defun m-bind (v f) 
  "Monadic BIND.  Unless dynamically shadowed, this is the identity BIND."
  (funcall f v))
(defun m-return (v) 
  "Monadic return.  Unless dynamically shadowed, this is the identity RETURN."
  v)

(defvar monad-state 
  (tbl! 
   :m-return (fn [x] (fn [s] (list x s)))
   :m-bind (fn [mv f] 
			   (fn [s] 
				   (dlet [[val new-state] (funcall mv s)]
					 (funcall (funcall f val) new-state)))))
  "The STATE monad.  Constructs a function which takes a state and
transforms it out of other such functions.")

(defvar monad-cont 
  (tbl! 
   :m-return (fn [v]
				 (fn [c]
					 (funcall c v)))
   :m-bind 
   (fn [mv mf]
	   (fn [c]
		   (funcall mv (fn [v]
						   (funcall (mf v) c))))))
  "The continuation monad.  Construct a function which takes a continuation goes.")

(defn call-bind [[:: bind :m-bind] & args]
  (apply bind args))

(defn call-return [[:: return :m-return] & args]
  (apply return args))

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
  "Define a function of state using monad-state.  IE, bind the result of 
 (DOMONAD MONAD-STATE MONAD-FORMS ...BODY) to the function NAME."
  (let ((state (gensym "state")))
	`(defun ,name (,state)
	   (funcall
		(domonad monad-state ,monad-forms ,@body)
		,state))))

(defvar monad-seq 
  (tbl! 
   :m-zero (list)
   :m-return (lambda (x) (list x))
   :m-bind (lambda (v f) (apply #'append (mapcar f v))))
  "The list/sequence monad.  Combine computations over multiple possibilities.")





(defun monad-set (predicate)
  "Returns a SET-MONAD with PREDICATE semantics.  
This is similar to the sequence
monad, but only admits unique results under PREDICATE.

 (domonad (monad-set #'=)
          [x '(1 2 3)
           y '(1 2 3)]
      (+ x y))

 yields: (2 3 4 5 6)

 (domonad monad-seq
          [x '(1 2 3)
           y '(1 2 3)]
      (+ x y))
 
 yields: (2 3 4 3 4 5 4 5 6)

"
  (lexical-let ((lpred predicate))
	(tbl! 
	 :m-zero (list)
 	 :m-return (lambda (x) (list x))
	 :m-bind (lambda (v f) (unique (apply #'append (mapcar f v)) lpred)))))

(defun m-m-bind (monad v f)
  "Call the bind function in MONAD with args V and F."
  (funcall (tbl monad :m-bind) v f))

(defun m-m-return (monad v)
  "Call the RETURN function in MONAD with V."
  (funcall (tbl monad :m-return) v))


(defmacro* with-monad (monad &body body)
  `(let ((current-monad ,monad))
	 (labels ((m-return (x) (m-m-return current-monad x))
			  (m-bind (v f) (m-m-bind current-monad v f))
			  (>>= (v f) (m-m-bind current-monad v f)))
	   (lexical-let ((m-zero (tbl current-monad :m-zero)))
		 ,@body))))

(defmacro* with-monad-dyn (monad &body body)
  `(let ((current-monad ,monad))
	 (flet ((m-return (x) (m-m-return current-monad x))
			(m-bind (v f) (m-m-bind current-monad v f))
			(>>= (v f) (m-m-bind current-monad v f)))
	   (let ((m-zero (tbl current-monad :m-zero)))
		 ,@body))))

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


(defmacro* domonad-helper* (forms &body body)
  (cond 
   ((= 0 (length forms)) `(m-return (progn ,@body)))
   (t 
	(dlet_ [[form val & rest-forms] forms]
	  `(m-bind ,val (fn ,(vector form) (domonad-helper* ,rest-forms ,@body)))))))

(defmacro* domonad* (monad forms &body body)
  (cond 
   ((oddp (length forms)) (error "domonad requires an even number of forms"))
   (t
	`(with-monad ,monad
				 (domonad-helper* ,forms ,@body)))))




(defmacro* domonad-inner (m-bind-sym m-return-sym forms &body body)
  (cond ((= 0 (length forms)) `(funcall ,m-return-sym (progn ,@body)))
		((>= (length forms) 2)
		 (dlet [[form val & rest-forms] forms]
		   `(funcall ,m-bind-sym 
					 ,val
					 (fnc ,(vector form)
						  (domonad-inner 
						   ,m-bind-sym 
						   ,m-return-sym 
						   ,rest-forms
						   ,@body)))))
		(t (error "domonad requires an even number of m-bind forms"))))


(defmacro* domonad (monad forms &body body)
  "DOMONAD: Sequence the computations/bindings in 
FORMS using the monad MONAD, finally, evaluate BODY,
returning the value of the final form.

Inside a DOMONAD M-BIND and M-RETURN are bound to 
the bind and return functions associated with MONAD.
Monads are pretty cool.

Note: >>= is also bound as the bind function.  This is handy
to combine with the $ infix macro: ($ v >>= f) -> (>>= v f) 
but resembles Haskell notation for the confused."
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
		 (labels ((m-bind (v f) (funcall ,m-bind-sym v f))
				  (>>= (v f) (funcall ,m-bind-sym v f))
				  (m-return (x) (funcall ,m-return-sym x)))
		   (funcall ,m-bind-sym
										;(funcall ,m-return-sym ,val)
					,val
					(fnc ,(vector form)
						 (domonad-inner 
						  ,m-bind-sym 
						  ,m-return-sym 
						  ,rest-forms 
						  ,@body)))))))))



(defn maybe+ [x y]
  (Just (+ x y)))

(defn maybe/ [x y]
  (if (= 0 y) (None)
	(Just (/ x y))))

										; transformers - more than meets the eye.

(defun m-seq (vlst)
  "Combine the monadic values in VLST into a monadic value using
the rules of the current monad."
  (reduce 
   (lambda (output v)
	 (m-bind v 
			 (lambda (x)
			   (m-bind output (lambda (y) 
								(m-return (cons x y)))))))
   (reverse vlst)
   :initial-value (m-return nil)))

(defun m-mapcar (f xs)
  "Map F across the monadic values in XS, combining the results
monadically, according to the current monad."
  (m-seq (mapcar f xs)))

(example
 (with-monad-dyn monad-seq
				 (m-seq 
				  '((a b) (c d)))
				 (m-mapcar (lambda (x) (list (+ x 1) (- x 1)))
						   '(1 2 3 4 5))))

(defvar current-monad monad-id "The identity is the current monad by default.")

(defun gen-m-lift-binding (arg-names)
  "Generate the temporary variable names for a lift."
  (coerce (loop for a in arg-names append (list a a)) 'vector))

(defmacro m-lift (n f)
  "Macro - LIFT F (with N args) into the current monad."
  (with-gensyms 
   (fsym)
   (let ((arg-names 
		  (loop for i from 1 to n collect
				(gensymf "arg%d" i))))
	 `(lexical-let ((,fsym ,f))
		(lex-lambda ,arg-names
					(domonad current-monad 
							 ,(gen-m-lift-binding arg-names)
							 (funcall ,fsym ,@arg-names)))))))

(defun m-lift1 (f)
  (m-lift 1 f))

(defun m-lift2 (f)
  (m-lift 2 f))

(defun m-lift3 (f)
  (m-lift 3 f))

(defun m-lift4 (f)
  (m-lift 4 f))

(defun m-lift5 (f)
  (m-lift 5 f))

(defun m-lift6 (f)
  (m-lift 6 f))

(defun lift-left (f)
  (lexical-let ((f f))
	(lambda (left &rest rest)
	  (domonad current-monad 
			   [left left]
			   (apply f left rest)))))

(defun lift-right (f)
  (lexical-let ((f f))
	(lambda (&rest rest)
	  (lexical-let* ((rrest (reverse rest))
					 (right (car rrest))
					 (rest (reverse (cdr rrest))))
		(domonad current-monad 
				 [right right]
				 (apply f (suffix rest right)))))))

(defun lift-nth (f n)
  (lexical-let ((f f) (n n))
	(lambda (&rest rest)
	  (let ((nth-item (elt rest n)))
		(domonad current-monad
				 [nth-item nth-item]
				 (setf (elt rest n) nth-item)
				 (apply f rest))))))

(provide 'monads)





