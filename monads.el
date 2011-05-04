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

(eval-when-compile-also 
 (defun monad? (m)
   (and (hash-table-p m)
		(tbl m :m-bind)
		(tbl m :m-return))))


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

(defvar monad-maybe^i
  (tbl!
   :m-zero nil
   :m-return (lambda (x) x)
   :m-bind (lambda (v f)
			 (if (not v) v
			   (funcall f v))))
  "The (implicit) MAYBE monad.  NIL indicates failure.  MaybeVal is the identity.  Just is the identity.")

(defun m-Error (arg)
  `(Error ,arg))

(defvar monad-error 
  (tbl! 
   :m-return (lambda (x) (Just x))
   :m-bind (lambda (v f) 
			 (if (eq (car v) 'Error) v
			   (funcall f (MaybeVal v))))))

(defun call-bind (monad mv mf)
  (funcall (tbl monad :m-bind mv mf)))

(defun call-return (monad val)
  (funcall (tbl monad :m-return) val))



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
  "The continuation monad.  Construct a function which takes a continuation.")

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

(defun map-cat-or-suffix (f lst)
  "Like mapcat, but turns non-list elements into lists if they are encountered."
  (loop for item in 
		(if (listp lst) lst 
		  (list lst))
		append
		(let ((result (funcall f item)))
		  (if (listp result) result
			(list result)))))

(defvar monad-seq^i
  (tbl! 
   :m-zero (list)
   :m-return (lambda (x) (list x))
   :m-bind (lambda (v f) (map-cat-or-suffix f v)))
  "The implicit list/sequence monad.  Combine computations over
  multiple possibilities.  Bind handles promoting single results
  to lists.  If you want to include a list, you have to m-return
  it explicitly. ")

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

(defun map-cat-or-suffix-set (f lst predicate)
  (let ((memo-table (make-hash-table :test predicate)))
	(flet ((memo (item)
				 (puthash item t memo))
		   (not-seen (item)
					 (if (not (gethash item memo-table))
						 (prog1 t (puthash item t memo-table))
					   nil)))
	  (loop for item in
			(if (listp lst) lst (list lst))
			append
			(let ((res (funcall f item)))
			  (cond ((listp res)
					 (filter #'not-seen res))
					(t
					 (if (not-seen res) (list res) nil))))))))


(defun monad-set^i (predicate)
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
	 :m-bind (lambda (v f) (map-cat-or-suffix-set f v lpred)))))


(defun m-m-bind (monad v f)
  "Call the bind function in MONAD with args V and F."
  (funcall (tbl monad :m-bind) v f))

(defun m-m-return (monad v)
  "Call the RETURN function in MONAD with V."
  (funcall (tbl monad :m-return) v))


(defmacro* with-monad (monad &body body)
  `(lexical-let-monad ,monad ,@body))

(defmacro* with-monad-dyn (monad &body body)
  `(let-monad ,monad ,@body))

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

;; (defn m-chain [steps]
;;   (foldl 
;;    (fn [step chain-expr]
;; 	   (fn [v] (m-bind (funcall chain-expr v) step)))
;;    #'m-return
;;    steps))

(defun m-chain2 (v1 v2)
  (lexical-let ((v1 v1)
				(v2 v2))
	(lambda (init)
	  (m-bind (m-bind init v2) v1))))
(defun m-chain (&rest vs)
  (let* ((rvs (reverse vs))
		 (chain (car rvs)))
	(loop for f in (cdr rvs) do
		  (setq chain (m-chain2 f chain))
		  finally (return chain))))    

;;;


(defmacro* let-monad (monad &rest body)
  "Create a dynamic scope in which MONAD is exposed
as CURRENT-MONAD, with M-ZERO and functions M-PLUS, M-BIND, M-RETURN and >>= (bind)
defined via let and flet forms.  Useing this inside LEXICAL-LET-MONAD is undefined."
  `(let* ((current-monad ,monad)
		  (m-zero (tbl current-monad :m-zero)))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in an mlet or similar form.  
A monad is a hash table with m-return and m-bind forms."))
	 (flet ((m-bind (v f)
					(funcall (tbl current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl current-monad :m-return) v))
			(>>= (v f)
				 (funcall (tbl current-monad :m-bind) v f))
			(m-plus (mv1 mv2)
					(funcall (tbl current-monad :m-plus) mv1 mv2)))
	   ,@body)))

(defmacro* lexical-let-monad (monad &rest body)
  "Create a LEXICAL scope in which MONAD is exposed
as CURRENT-MONAD, with M-ZERO and functions M-PLUS, M-BIND, M-RETURN and >>= (bind)
defined via lexical-let and LABELS."

  `(lexical-let* ((current-monad ,monad)
				  (m-zero (tbl current-monad :m-zero)))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in an mlet or similar form.  
A monad is a hash table with m-return and m-bind forms."))
	 (labels ((m-bind (v f)
					  (funcall (tbl current-monad :m-bind) v f))
			  (m-return (v)
						(funcall (tbl current-monad :m-return) v))
			  (>>= (v f)
				   (funcall (tbl current-monad :m-bind) v f))
			  (m-plus (mv1 mv2)
					  (funcall (tbl current-monad :m-plus) mv1 mv2)))
	   ,@body)))

(defmacro* lexical-domonad-inner (binders &rest body)
  (if (empty? binders) `(progn ,@body)
	(dlet_ [[sym expr & rest-binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (lexical-domonad-inner ,rest-binders ,@body))))))

(defmacro* lexical-domonad ({monad} binders &body body)
  "LEXICAL-DOMONAD - sequence binders (a clojure vector binding expression) through
{MONAD}, which is the current dynamically scoped monad, if not supplied.  
Finally execute and return BODY.  BODY and BINDERS have LEXICALLY scoped copies 
of the monad and associated functions."
  (if (vectorp {monad})
	  `(lexical-domonad current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (lexical-let-monad 
	   current-monad 
	   (lexical-domonad-inner ,binders ,@body)))))


(defmacro* domonad-inner (binders &rest body)
  (if (empty? binders) `(progn ,@body)
	(dlet_ [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (domonad-inner ,binders ,@body))))))

(defmacro* domonad ({monad} binders &body body)
  "DOMONAD - sequence binders (a clojure vector binding expression) through
{MONAD}, which is the current dynamically scoped monad, if not supplied.  
Finally execute and return BODY.  BODY and BINDERS have DYNAMICALLY scoped copies 
of the monad and associated functions.

Use this form if you wish to define a function which is MONAD independent."

  (if (vectorp {monad})
	  `(domonad current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (domonad-inner ,binders ,@body))))


(defmacro* domonad-inner< (binders &rest body)

  (if (empty? binders) `(m-return (progn ,@body))
	(dlet_ [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (domonad-inner< ,binders ,@body))))))

(defmacro* domonad< ({monad} binders &body body)
  "DOMONAD - sequence binders (a clojure vector binding expression) through
{MONAD}, which is the current dynamically scoped monad, if not supplied.  
Finally execute and return BODY, wrapping the result with M-RETURN.  
BODY and BINDERS have DYNAMICALLY scoped copies 
of the monad and associated functions.

Use this form if you wish to define a function which is MONAD independent.

This form corresponds most directly to the Clojure DOMONAD form."
  (if (vectorp {monad})
	  `(domonad< current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (domonad-inner< ,binders ,@body))))

(defmacro* lexical-domonad-inner< (binders &rest body)
  (if (empty? binders) `(m-return (progn ,@body))
	(dlet_ [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (lexical-domonad-inner< ,binders ,@body))))))

(defmacro* lexical-domonad< ({monad} binders &body body)
  "LEXICAL-DOMONAD< - sequence binders (a clojure vector binding expression) through
{MONAD}, which is the current dynamically scoped monad, if not supplied.  
Finally execute and return BODY, wrapping the result with M-RETURN.  
BODY and BINDERS have LEXICALLY scoped copies 
of the monad and associated functions.

This is the most heavy duty form.
"
  (if (vectorp {monad})
	  `(lexical-domonad< current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (lexical-let-monad 
	   current-monad 
	   (lexical-domonad-inner< ,binders ,@body)))))

(defmacro* lexical-mlet-inner (binders &rest body)

  (cond 
   ((empty? binders) `(progn ,@body))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (,symbol)
						   (lexical-mlet-inner ,(cdr binders) ,@body)))))))

(defmacro* lexical-mlet (monad binders &rest body)
"LEXICAL-MLET - Chain the operations in BINDERS, regular 
lisp style let binding expressions, through the monad MONAD,
finally returning the result of BODY.  Lexically bound copies
of the monad and monad functions are provided in the expression
forms of this macro."
  `(let-monad ,monad
			  (lexical-let-monad current-monad 
								   (lexical-mlet-inner ,binders ,@body))))

(defmacro* lexical-mlet-inner< (binders &rest body)
  (cond 
   ((empty? binders) `(m-return (progn ,@body)))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (,symbol)
						   (lexical-mlet-inner< ,(cdr binders) ,@body)))))))

(defmacro* lexical-mlet< (monad binders &rest body)
"LEXICAL-MLET - Chain the operations in BINDERS, regular 
lisp style let binding expressions, through the monad MONAD,
finally returning the result of BODY, wrapped in a final call 
to M-RETURN.  

Lexically bound copies
of the monad and monad functions are provided in the expression
forms of this macro."

  `(let ((current-monad ,monad))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in lexical-mlet< or similar form.  A monad is a hash table with m-return and m-bind forms."))
	 (let-monad ,monad
				(lexical-let-monad current-monad 
								   (lexical-mlet-inner< ,binders ,@body)))))

(defmacro* mlet-inner (binders &rest body)
  (cond 
   ((empty? binders) `(progn ,@body))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (,symbol)
						   (mlet-inner ,(cdr binders) ,@body)))))))

(defmacro* mlet (monad binders &rest body)
"MLET - Monadic let.  Sequence the bindings represented in BINDINGS, 
which resemble regular lisp let-like binding forms, through the monad
MONAD.  Finally execute and return body.

This is the most emacs-lisp flavored monad form."
  `(let-monad ,monad
			  (mlet-inner ,binders ,@body)))

(defmacro* mlet-inner< (binders &rest body)
  (cond 
   ((empty? binders) `(m-return (progn ,@body)))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (,symbol)
						   (mlet-inner< ,(cdr binders) ,@body)))))))

(defmacro* mlet< (monad binders &rest body)
  "MLET - Monadic let.  Sequence the bindings represented in BINDINGS, 
which resemble regular lisp let-like binding forms, through the monad
MONAD.  Finally execute and return body, wrapped in a final M-RETURN."
  (if (not (monad? current-monad))
	  (error "Expected a monad in mlet or similar form.  A monad is a hash table with m-return and m-bind forms."))
  `(let-monad ,monad
			  (mlet-inner< ,binders ,@body)))

(defun tagged-value? (tag val)
  (and (listp val)
	   (eq (car val) tag)))

(lex-defun tagged-monad (tag)
  (tbl! 
   :m-bind (lex-lambda (v f)
					   (if (not (tagged-value? tag v))
						   (error "Tagged monad error, expected tagged value of tag %s" tag))
					   (funcall f (cadr v)))
   :m-return (lambda (v) (list tag v))))




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
  "Map F across the values in XS, combining the results
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

(defmacro* m-lift (n f)
  "Macro - LIFT F (with N args) into the current monad."
  (let ((arg-names (mapcar (pal #'gensymf "lift-arg%d-") (range n))))
	(with-gensyms 
 	 (f-to-lift)
 	 `(lexical-let ((,f-to-lift ,f))
		(lambda ,arg-names
		  (mlet< current-monad
				 ,(loop for nm in arg-names collect
						`(,nm ,nm))
				 (funcall ,f-to-lift ,@arg-names))))))) 

(defmacro* m-lift-into (n f monad)
  "Macro - LIFT F (with N args) into the current monad."
  (with-gensyms (lifted-args)
				`(lambda (&rest ,lifted-args)
				   (let-monad ,monad 
							  (apply 
							   (m-lift ,n ,f) ,lifted-args)))))

(defun m-lift-into1 (f monad) 
  (m-lift-into 1 f monad))
(defun m-lift-into2 (f monad) 
  (m-lift-into 2 f monad))
(defun m-lift-into3 (f monad) 
  (m-lift-into 3 f monad))
(defun m-lift-into4 (f monad) 
  (m-lift-into 4 f monad))
(defun m-lift-into5 (f monad) 
  (m-lift-into 5 f monad))
(defun m-lift-into6 (f monad) 
  (m-lift-into 6 f monad))

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





