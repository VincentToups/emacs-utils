;;* ("lexical-domonad<" 
;;*  "lexical-mlet<"
;;  "domonad<"
;;*  "mlet<"
;;*  "lexical-domonad"
;;*  "lexical-mlet"
;;  "domonad"
;;*  "mlet")

(defun monad? (m)
  (and (hash-table-p m)
	   (tbl m :m-bind)
	   (tbl m :m-return)))


(defmacro let-monad (monad &rest body)
  `(let ((current-monad ,monad)
		 (m-zero (tbl current-monad :m-zero)))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in an mlet or similar form.  
A monad is a hash table with m-return and m-bind forms."))
	 (flet ((m-bind (v f)
					(funcall (tbl current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl current-monad :m-return) v))
			(>>= (v f)
				 (funcall (tbl current-monad) :m-bind) v f)
			(m-plus (mv mv)
					(funcall (tbl current-monad) :m-plus mv mv)))
	   ,@body)))

(defmacro lexical-let-monad (monad &rest body)
  `(lexical-let ((current-monad ,monad)
				 (m-zero (tbl current-monad :m-zero)))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in an mlet or similar form.  
A monad is a hash table with m-return and m-bind forms."))
	 (labels ((m-bind (v f)
					  (funcall (tbl current-monad :m-bind) v f))
			  (m-return (v)
						(funcall (tbl current-monad :m-return) v))
			  (>>= (v f)
				   (funcall (tbl current-monad) :m-bind) v f)
			  (m-plus (mv mv)
					  (funcall (tbl current-monad) :m-plus mv mv)))
	   ,@body)))

(defmacro lexical-domonad-inner (binders &rest body)
  (if (empty? binders) `(progn ,@body)
	(dlet [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (lexical-domonad-inner ,binders ,@body))))))

(defmacro lexical-domonad ({monad} binders &body body)
  (if (vectorp {monad})
	  `(lexical-domonad current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (lexical-let-monad 
	   current-monad 
	   (lexical-domonad-inner binders ,@body)))))


(defmacro domonad-inner (binders &rest body)
  (if (empty? binders) `(progn ,@body)
	(dlet [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (domonad-inner ,binders ,@body))))))

(defmacro domonad ({monad} binders &body body)
  (if (vectorp {monad})
	  `(domonad current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (domonad-inner binders ,@body))))


(defmacro domonad-inner< (binders &rest body)
  (if (empty? binders) `(m-return (progn ,@body))
	(dlet [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (domonad-inner< ,binders ,@body))))))

(defmacro domonad< ({monad} binders &body body)
  (if (vectorp {monad})
	  `(domonad< current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (domonad-inner< binders ,@body))))

(defmacro lexical-domonad-inner< (binders &rest body)
  (if (empty? binders) `(m-return (progn ,@body))
	(dlet [[sym expr & binders] binders]
	  `(m-bind ,expr (fn ,(vector sym)
						 (lexical-domonad-inner< ,binders ,@body))))))

(defmacro lexical-domonad< ({monad} binders &body body)
  (if (vectorp {monad})
	  `(lexical-domonad< current-monad ,{monad} ,@(cons binders body))
	`(let-monad 
	  ,{monad} 
	  (lexical-let-monad 
	   current-monad 
	   (lexical-domonad-inner< binders ,@body)))))

(defmacro lexical-mlet-inner (binders &rest body)
  (cond 
   ((empty? binders) `(progn ,@body))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (symbol)
						   (lexical-mlet-inner ,(cdr binders) ,@body)))))))

(defmacro lexical-mlet (monad binders &rest body)
  `(let ((current-monad ,monad))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in lexical-mlet or similar form.  A monad is a hash table with m-return and m-bind forms."))
	 (let-monad ,monad
				(lexical-let-monad ,current-monad 
								   (lexical-mlet-inner ,binders ,@body)))))

(defmacro lexical-mlet-inner< (binders &rest body)
  (cond 
   ((empty? binders) `(m-return (progn ,@body)))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (symbol)
						   (lexical-mlet-inner< ,(cdr binders) ,@body)))))))

(defmacro lexical-mlet< (monad binders &rest body)
  `(let ((current-monad ,monad))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in lexical-mlet< or similar form.  A monad is a hash table with m-return and m-bind forms."))
	 (let-monad ,monad
				(lexical-let-monad ,current-monad 
								   (lexical-mlet-inner< ,binders ,@body)))))

(defmacro mlet-inner (binders &rest body)
  (cond 
   ((empty? binders) `(progn ,@body))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (symbol)
						   (mlet-inner ,(cdr binders) ,@body)))))))

(defmacro mlet (monad binders &rest body)
  `(let ((current-monad ,monad))
	 (if (not (monad? current-monad))
		 (error "Expected a monad in mlet or similar form.  A monad is a hash table with m-return and m-bind forms."))
	 (flet ((m-bind (v f)
					(funcall (tbl ,current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl ,current-monad :m-return) v))
			(>>= (v f)
				 (funcall (tbl ,current-monad) :m-bind) v f))
	   (mlet-inner ,binders ,@body))))

(defmacro mlet-inner< (binders &rest body)
  (cond 
   ((empty? binders) `(m-return (progn ,@body)))
   (t 
	(let* ((binder (car binders))
		   (symbol (car binder))
		   (expr   (cadr binder)))
	  `(m-bind ,expr
			   (lex-lambda (symbol)
						   (mlet-inner< ,(cdr binders) ,@body)))))))

(defmacro mlet< (monad binders &rest body)
  (if (not (monad? current-monad))
	  (error "Expected a monad in mlet or similar form.  A monad is a hash table with m-return and m-bind forms."))
  `(let ((current-monad ,monad))
	 (flet ((m-bind (v f)
					(funcall (tbl ,current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl ,current-monad :m-return) v))
			(>>= (v f)
				 (funcall (tbl ,current-monad) :m-bind) v f))
	   (mlet-inner< ,binders ,@body))))

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



OLD SHIT
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
	`(with-monad-dyn ,monad
				 (domonad-helper* ,forms ,@body)))))

(defmacro* domonad** (monad forms &body body)
  "Like DOMONAD but does not warp the BODY of the macro in an M-RETURN."
  (cond 
   ((oddp (length forms)) (error "domonad requires an even number of forms"))
   (t
	`(with-monad-dyn ,monad (with-monad ,monad
				 (domonad-helper* ,forms ,@body))))))

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

(defmacro* mlet** (monad binders &body body)
  "Performs the bindings in BINDERS in the monad MONAD, finally
executing BODY in an implicit m-return.  In the dynamic contex,
m-return, m-bind and m-zero are bound.  >>= is a synonym for
m-bind.  Binders are standard elisp binder syntax, just like a
let*.  This also lexical-lets the monad functions so that delayed
compuations behave apropriately, even without a domonad
enclosure."
  `(let ((current-monad ,monad))
	 (lexical-let ((current-monad current-monad))
	   (labels ((m-bind (v f) 
					  (funcall (tbl current-monad :m-bind) v f))
			  (>>= (v f)
				   (funcall (tbl current-monad :m-bind) v f))
			  (m-return (v)
						(funcall (tbl current-monad :m-return) v)))
		 (flet ((m-bind (v f) 
						  (funcall (tbl current-monad :m-bind) v f))
				  (>>= (v f)
					   (funcall (tbl current-monad :m-bind) v f))
				  (m-return (v)
							(funcall (tbl current-monad :m-return) v)))
		   (mlet*-inner ,binders ,@body))))))


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



(defmacro* mlet**_ (monad binders &body body)
  "Exactly like mlet* except that body is not wrapped in an implicit m-return."
  `(let ((current-monad ,monad))
	 (lexical-let ((current-monad current-monad))
	 (flet ((m-bind (v f) 
					(funcall (tbl current-monad :m-bind) v f))
			(>>= (v f)
				 (funcall (tbl current-monad :m-bind) v f))
			(m-return (v)
					  (funcall (tbl current-monad :m-return) v)))
	   (labels ((m-bind (v f) 
						 (funcall (tbl current-monad :m-bind) v f))
				(>>= (v f)
					  (funcall (tbl current-monad :m-bind) v f))
				(m-return (v)
						   (funcall (tbl current-monad :m-return) v)))
		 (mlet*_-inner ,binders ,@body))))))


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
