(require 'cl)
(require 'utils)
(require 'parse-table-binder)
(require 'parse-seq-binder)

(setq currently-defining-defn 'lambda)

(defun binder->type (f)
  (cond ((symbolp f) :symbol)
		((and (vectorp f)
			  (not (eq ':: (elt f 0))))
		 :seq)
		((and (vectorp f)
			  (eq (elt f 0) '::))
		 :tbl)
		(t (error "Unrecognized form type"))))

(defun forms->binders (fs)
  (loop for i from 0 below (length fs) 
		when (evenp i)
		collect (elt fs i)))
(defun forms->expressions (fs)
  (loop for i from 0 below (length fs) 
		when (oddp i)
		collect (elt fs i)))

(defun handle-seq-binder (binder expr previous-lets)
  (let-seq
   (sub-binders rest-form as-sym or-form) (parse-and-check-seq-binder binder)
   (let 
	((previous-lets (append previous-lets (list 
										  (vector as-sym expr)))))
	(if rest-form 
		(setf previous-lets
			  (append previous-lets 
					  (handle-binding rest-form 
							  `(coerce (nthcdr ,(length sub-binders)
											   (coerce ,as-sym 'list))
									   (if (listp ,as-sym) 'list 'vector))))))
	(append 
	 previous-lets
	 (loop for i from 0 to (length sub-binders)
		   and ind in sub-binders 
		   append
		   (handle-binding ind `(elt ,as-sym ,i)))))))

(defun wrap-or-form (form)
  `(lambda () ,form))

(defun handle-tbl-binding (binder expr previous-lets)
  (let-seq (sub-binders 
			keys
			as-sym
			or-form) (parse-and-check-tbl-binder binder)
	(let 	
		((or-form-name nil)
		 (previous-lets (append previous-lets (handle-binding as-sym expr))))
	  (if or-form
		  (progn 
			(setf or-form (wrap-or-form or-form))
			(setf or-form-name (gensym "or-form-name"))
			(setf previous-lets
				  (suffix previous-lets (vector or-form-name `(funcall ,or-form))))))
	  (append 
	   previous-lets
	   (loop for kw in keys
			 and sym in sub-binders
			 append
			 (if (not or-form)
				 (handle-binding sym `(tbl ,as-sym ,kw))
			   (handle-binding sym `(tbl-or ,as-sym ,kw (tbl ,or-form-name ,kw)))))))))

; (handle-tbl-binding [:: [a b] :x y :y :as table :or (tbl! :x [1 2])] '(tbl 'x 10 'y 11) '())

(defun* handle-binding (binder expr &optional
								(previous-lets '()))
  (case (binder->type binder)
	(:symbol (append previous-lets (list (vector binder expr))))
	(:seq (handle-seq-binder binder expr previous-lets))
	(:tbl (handle-tbl-binding binder expr previous-lets))))

; (handle-binding [a [:: [a b :as lst] :s :as table] & rest] 10)

(defun package-dlet-binders (pairs)
  (let ((n (length pairs))
		(binders '())
		(exprs '()))
	(loop for i from 0 below n by 2 
		  and j from 1 below n by 2 do
		  (push (elt pairs i) binders)
		  (push (elt pairs j) exprs)
		  finally
		  return
		  (list (coerce (reverse binders) 'vector)
				(cons 'list (reverse exprs))))))
(defun pairs->dlet-binding-forms (pairs)
  (mapcar (lambda (x) (coerce x 'list))
		  (apply #'handle-binding (package-dlet-binders pairs))))

; (package-dlet-binders [x 10 y 11 [a b] '(1 2)])
; (cl-prettyprint (pairs->dlet-binding-forms [x 10 y 11 [a b] '(1 2)]))


; (apply #'append (mapcar #'handle-binding (package-dlet-binders [x 10 y 11])))

(defmacro* dlet (pairs &body body)
  `(lexical-let* ,(pairs->dlet-binding-forms pairs) ,@body))

; (dlet [[a b] (list 10 10) y 11] (+ a b y))

(defun generate-defn-exprs (arg-nam n)
  (loop for i from 0 below n collect
		`(elt ,arg-nam ,i)))

(defun binder-arity (binder)
  (let-seq
   (sub-binders
	rest-form
	as-sym
	or-form) (parse-and-check-seq-binder binder)
   (let ((n (length sub-binders)))
	 (if rest-form (list n '+more)
	   (list n 'exactly)))))

; (binder-arity [a b c])

(defun arity-match (n arity)
  (if (eq (cadr arity) 'exactly)
	  (= n (car arity))
	(>= n (car arity))))

; (arity-match 2 '(3 +more))

(setq currently-defining-defn 'lambda)

(defmacro* fn (&rest rest)
	(cond
	 ((vectorp (car rest))
	  `(fn (,(car rest) ,@(cdr rest))))
	 ((listp (car rest))	   ; set of different arity binders/bodies
	  (let ((args-sym (gensym))
			(numargs (gensym)))
		`(lambda (&rest ,args-sym) 
		   (let ((,numargs (length ,args-sym)))
			 (cond
			  ,@(suffix (loop for pair in rest append
					  (let ((binders (car pair))
							(body (cdr pair)))
						(assert (vectorp binders) t (format "binder forms need to be vectors (error in %s)." currently-defining-defn))
						`(((arity-match ,numargs ',(binder-arity binders))
						   (lexical-let* ,(mapcar 
										   (lambda (x) (coerce x 'list)) 
										   (handle-binding binders args-sym)) ,@body)))))
					  `(t (error "Unable to find an arity match for %d args in fn %s." ,numargs ',currently-defining-defn))))))))
	 (t (error "Can't parse defn %s.  Defn needs a binder/body pair or a list of such pairs.  Neither appears to have been passed in. " currently-defining-defn))))

(defmacro* defn (name &rest rest)
  `(let ((currently-defining-defn ',name))
	 (fset ',name (fn ,@rest))))

; (defn f (x x) ([a b] (+ a b) ))

; (defn a-test-f [x y [:: z :z :as a-tble :as eh]] (list (+ x y z) a-tble))
; (defn a-test-f [x y [:: z :z :as a-tble]] (list (+ x y z) a-tble))
; (a-test-f 1 2 (tbl! :z 10))
; (f 1 2 3)
; (defn f ([z a] (* z a)) (x x) )

; (defn f [z [:: a :a :as a-table :or (tbl! :a 100)]] (list z a))
; (f 10 (tbl!))
	



