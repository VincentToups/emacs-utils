(require 'cl)
(require 'utils)
(require 'defn)
(require 'recur)

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

(defun m-Error (arg)
  `(Error ,arg))

(defvar monad-error 
  (tbl! 
   :m-return (lambda (x) (Just x))
   :m-bind (lambda (v f) 
			 (if (eq (car v) 'Error) v
			   (funcall f (MaybeVal v))))))



(defvar monad-id 
  (tbl! :m-return (lambda (x) x)
		:m-bind (lambda (v f) (funcall f v)))
  "The identity monad - you know, for kids.")

(defvar current-monad monad-id "The identity is the current monad by default.")

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
   ((= 0 (length forms))  `(progn ,@body))
   (t 
	(dlet_ [[form val & rest-forms] forms]
	  `(m-bind ,val (fn ,(vector form) (domonad-helper* ,rest-forms ,@body)))))))

(defmacro* domonad* (monad forms &body body)
  "Like DOMONAD but does not warp the BODY of the macro in an M-RETURN."
  (cond 
   ((oddp (length forms)) (error "domonad requires an even number of forms"))
   (t
	`(with-monad ,monad
				 (domonad-helper* ,forms ,@body)))))





(defmacro mlet*-inner (binders &rest body)
  "Handles inner expansion of mlet*."
  (if (empty? binders)
	  `(m-return (progn ,@body))
	(let* ((binder (car binders))
		   (sym (car binder))
		   (expr (cadr binder))
		   (rest-binders (cdr binders)))
	  `(m-bind ,(cadr binder)
			   (lambda (,sym)
				 (lexical-let ((,sym ,sym))
				   (mlet*-inner ,rest-binders ,@body)))))))

(defmacro* mlet* (monad binders &body body)
  "Performs the bindings in BINDERS in the monad MONAD, finally
executing BODY in an implicit m-return.  In the dynamic contex,
m-return, m-bind and m-zero are bound.  >>= is a synonym for
m-bind.  Binders are standard elisp binder syntax, just like a
let*."
  `(let ((current-monad ,monad))
	 (flet ((m-bind (v f) 
					(funcall (tbl current-monad :m-bind) v f))
			(>>= (v f)
				 (funcall (tbl current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl current-monad :m-return) v)))
	   (mlet*-inner ,binders ,@body))))

(defmacro mlet*_-inner (binders &rest body)
  (if (empty? binders)
	  `(progn ,@body)
	(let* ((binder (car binders))
		   (sym (car binder))
		   (expr (cadr binder))
		   (rest-binders (cdr binders)))
	  `(m-bind ,(cadr binder)
			   (lambda (,sym)
				 (lexical-let ((,sym ,sym))
				   (mlet*_-inner ,rest-binders ,@body)))))))

(defmacro* mlet*_ (monad binders &body body)
  "Exactly like mlet* except that body is not wrapped in an implicit m-return."
  `(let ((current-monad ,monad))
	 (flet ((m-bind (v f) 
					(funcall (tbl current-monad :m-bind) v f))
			(>>= (v f)
				 (funcall (tbl current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl current-monad :m-return) v)))
	   (mlet*_-inner ,binders ,@body))))

(defun check-monad-binders (binder)
  (if (not (vectorp binder)) (error "domonad-like forms need a vector for its bind forms."))
  (if (not (= 0 (mod (length binder) 2)))
	  (error "domonad-like vector binders need to have an even number of forms.")))

(defmacro* domonad-inner (binders &rest body)
  "Handles the simple expansion of the inner monad forms.
DOMONAD binds the monad functions."
  (if (empty? binders) `(m-return (progn ,@body))
	(dlet_ [[var expr & latter-binders] binders]
	  `(m-bind ,expr (fn ,(vector var)
						 (domonad-inner ,latter-binders ,@body))))))

(defmacro* domonad ({monad} binders &body body)
  "DOMONAD: Sequence the computations/bindings in FORMS using the
  monad MONAD, finally, evaluate BODY, returning the value of the
  final form.  FORMS are binders like those from the DEFN library,
  and are unnested compared to regular let bindings.  That is, [x
  10 y 11] binds x to 10, y to 11.

  Use mlet* for a standard lisp-like binder.

  Inside a DOMONAD M-BIND and M-RETURN are bound to 
  the bind and return functions associated with MONAD.
  Monads are pretty cool.

  Note: >>= is also bound as the bind function.  This is handy
  to combine with the $ infix macro: ($ v >>= f) -> (>>= v f) 
  but resembles Haskell notation for the confused.

  You may leave of {monad}, in which case, this runs in the
  current monad, defaulting to the identity."

  (if (vectorp {monad}) 
	  `(domonad current-monad ,binders ,@body)
	(progn
	  (check-monad-binders binders)
	  `(let* ((current-monad ,{monad})
			  (m-zero (tbl current-monad :m-zero)))
		 (flet ((m-bind (v f) 
						(funcall (tbl current-monad :m-bind) v f))
				(>>= (v f)
					 (funcall (tbl current-monad :m-bind) v f))
				(m-return (v)
						  (funcall (tbl current-monad :m-return) v)))
		   (domonad-inner ,binders ,@body))))))

(defmacro* domonad-inner_ (binders &rest body)
  "Handles the simple expansion of the inner monad forms.
DOMONAD binds the monad functions."
  (if (empty? binders) `(m-return (progn ,@body))
	(dlet_ [[var expr & latter-binders] binders]
	  `(m-bind ,expr (fn ,(vector var)
						 (domonad-inner_ ,latter-binders ,@body))))))

(defmacro* domonad_ ({monad} binders &body body)
  "Just like domonad but does not enclose BODY in an implicit return."
  (if (vectorp {monad}) 
	  `(domonad current-monad_ ,binders ,@body)
	(progn
	  (check-monad-binders binders)
	  `(let* ((current-monad ,{monad})
			  (m-zero (tbl current-monad :m-zero)))
		 (flet ((m-bind (v f) 
						(funcall (tbl current-monad :m-bind) v f))
				(>>= (v f)
					 (funcall (tbl current-monad :m-bind) v f))
				(m-return (v)
						  (funcall (tbl current-monad :m-return) v)))
		   (domonad-inner_ ,binders ,@body))))))


;; (defmacro* domonad-inner (m-bind-sym m-return-sym forms &body body)
;;   (cond ((= 0 (length forms)) `(funcall ,m-return-sym (progn ,@body)))
;; 		((>= (length forms) 2)
;; 		 (dlet [[form val & rest-forms] forms]
;; 		   `(funcall ,m-bind-sym 
;; 					 ,val
;; 					 (fnc ,(vector form)
;; 						  (domonad-inner 
;; 						   ,m-bind-sym 
;; 						   ,m-return-sym 
;; 						   ,rest-forms
;; 						   ,@body)))))
;; 		(t (error "domonad requires an even number of m-bind forms"))))


;; (defmacro* domonad (monad forms &body body)
;;   "DOMONAD: Sequence the computations/bindings in FORMS using the
;; monad MONAD, finally, evaluate BODY, returning the value of the
;; final form.  FORMS are binders like those from the DEFN library,
;; and are unnested compared to regular let bindings.  That is, [x
;; 10 y 11] binds x to 10, y to 11.

;; Use mlet* for a standard lisp-like binder.

;; Inside a DOMONAD M-BIND and M-RETURN are bound to 
;; the bind and return functions associated with MONAD.
;; Monads are pretty cool.

;; Note: >>= is also bound as the bind function.  This is handy
;; to combine with the $ infix macro: ($ v >>= f) -> (>>= v f) 
;; but resembles Haskell notation for the confused."
;;   (cond
;;    ((oddp (length forms)) (error "domonad requires an even number of forms")) 
;;    ((= 0 (length forms)) `(progn ,@body))
;;    ((>= (length forms) 2)
;; 	(dlet [monad-sym 
;; 		   (gensym "monad")
;; 		   m-bind-sym 
;; 		   (gensym "m-bind")
;; 		   m-return-sym 
;; 		   (gensym "m-return")
;; 		   [form val & rest-forms]
;; 		   forms]
;; 	  `(dlet [,monad-sym
;; 			  ,monad
;; 			  ,m-bind-sym
;; 			  (tbl ,monad-sym :m-bind)
;; 			  ,m-return-sym
;; 			  (tbl ,monad-sym :m-return)]
;; 		 (labels ((m-bind (v f) (funcall ,m-bind-sym v f))
;; 				  (>>= (v f) (funcall ,m-bind-sym v f))
;; 				  (m-return (x) (funcall ,m-return-sym x)))
;; 		   (funcall ,m-bind-sym
;; 										;(funcall ,m-return-sym ,val)
;; 					,val
;; 					(fnc ,(vector form)
;; 						 (domonad-inner 
;; 						  ,m-bind-sym 
;; 						  ,m-return-sym 
;; 						  ,rest-forms 
;; 						  ,@body)))))))))



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

(defmacro m-lift-into (n f monad)
  "Macro - LIFT F (with N args) into the current monad."
  (with-gensyms 
   (fsym monadsym)
   (let ((arg-names 
		  (loop for i from 1 to n collect
				(gensymf "arg%d" i))))
	 `(lexical-let ((,fsym ,f)
					(,monadsym ,monad))
		(lex-lambda ,arg-names
					(domonad ,monadsym
							 ,(gen-m-lift-binding arg-names)
							 (funcall ,fsym ,@arg-names)))))))

(defun m-lift-into1 (f monad) (m-lift-into 1 f monad))
(defun m-lift-into2 (f monad) (m-lift-into 2 f monad))
(defun m-lift-into3 (f monad) (m-lift-into 3 f monad))
(defun m-lift-into4 (f monad) (m-lift-into 4 f monad))
(defun m-lift-into5 (f monad) (m-lift-into 5 f monad))
(defun m-lift-into6 (f monad) (m-lift-into 6 f monad))

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

;; (defun lift-left (f)
;;   (lexical-let ((f f))
;; 	(lambda (left &rest rest)
;; 	  (domonad current-monad 
;; 			   [left left]
;; 			   (apply f left rest)))))

;; (defun lift-right (f)
;;   (lexical-let ((f f))
;; 	(lambda (&rest rest)
;; 	  (lexical-let* ((rrest (reverse rest))
;; 					 (right (car rrest))
;; 					 (rest (reverse (cdr rrest))))
;; 		(domonad current-monad 
;; 				 [right right]
;; 				 (apply f (suffix rest right)))))))

;; (defun lift-nth (f n)
;;   (lexical-let ((f f) (n n))
;; 	(lambda (&rest rest)
;; 	  (let ((nth-item (elt rest n)))
;; 		(domonad* current-monad
;; 				  [nth-item nth-item]
;; 				  (setf (elt rest n) nth-item)
;; 				  (apply f rest))))))




(provide 'monads)





