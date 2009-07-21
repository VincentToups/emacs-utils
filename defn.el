(require 'cl)
(require 'utils)
(require 'parse-table-binder)
(require 'parse-seq-binder)

(setq currently-defining-defn 'lambda)

(defun binder->type (f)
  (cond
   ((and 
	 (vectorp f)
	 (= 0 (length f)))
	:seq)
   ((symbolp f) :symbol)
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

(defun wrap-or-form (form)
  `(lambda () ,form))

(defun handle-seq-binder (binder expr previous-lets)
  (let-seq
   (sub-binders rest-form as-sym or-form) (parse-and-check-seq-binder binder)
   (if (not as-sym) (setf as-sym (gensym (format "%s-seq-as-sym" currently-defining-defn))))
   (let 
	   ((or-form-name nil)
		(previous-lets (append previous-lets (list 
											  (vector as-sym expr)))))
	 (if or-form
		 (progn 
		   (setf or-form (wrap-or-form or-form))
		   (setf or-form-name (gensym "or-form-name"))
		   (setf previous-lets
				 (suffix previous-lets (vector or-form-name `(funcall ,or-form))))))
	 (if rest-form 
		 (setf previous-lets
			   (append previous-lets 
					   (handle-binding rest-form 
									   (if or-form
										   `(nthcdr-preserve-type ,(length sub-binders)
																  (transplant-tail ,as-sym ,or-form-name))
										 `(nthcdr-preserve-type ,(length sub-binders)
																,as-sym))))))
	 
	 (append 
	  previous-lets
	  (loop for i from 0 to (length sub-binders)
			and ind in sub-binders 
			append
			(handle-binding ind 
							(if or-form 
								`(elt-or ,as-sym ,i (elt ,or-form-name ,i))
							  `(elt ,as-sym ,i))))))))

										; (handle-seq-binder [a b c d :or [1 2 3 4]] '(list 1 2 3 4) '())
										; (handle-seq-binder [] '() '())


(defun handle-tbl-binding (binder expr previous-lets)
  (let-seq (sub-binders 
			keys
			as-sym
			or-form
			keys-seq) (parse-and-check-tbl-binder binder)
			(if (not as-sym) (setf as-sym (gensym (format "%s-as-symbol" currently-defining-defn))))
			(let 	
				((or-form-name nil)
				 (previous-lets (append previous-lets (handle-binding as-sym expr))))
			  (if or-form
				  (progn 
					(setf or-form (wrap-or-form or-form))
					(setf or-form-name (gensym "or-form-name"))
					(setf previous-lets
						  (suffix previous-lets (vector or-form-name `(funcall ,or-form))))))

			  (if keys-seq
				  (loop for s across keys-seq do
						(setf keys (suffix keys (make-keyword (format "%s" s))))
						(setf sub-binders (suffix sub-binders s))))
			  (append 
			   previous-lets
			   (loop for kw in keys
					 and sym in sub-binders
					 append
					 (if (not or-form)
						 (handle-binding sym `(tbl ,as-sym ,kw))
					   (handle-binding sym `(tbl-or ,as-sym ,kw (tbl ,or-form-name ,kw)))))))))

										; (handle-tbl-binding [:: [a b] :x y :y :as table :or (tbl! :x [1 2])] '(tbl 'x 10 'y 11) '())
										; (handle-tbl-binding [:: [a b :as q] :x :keys [y z]] '(tbl :x 10 :y 11 :z 14) '())

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


(defun split-after-two (lst)
  (list (elts lst (range 2))
        (elts lst (range 2 (length lst)))))

										; (split-after-two '(1 2))
										; (split-after-two '(1 2 3 4 5 6 7 8))
										; (split-after-two [1 2 3 4 5 6])

(defmacro* dlet (pairs &body body)
  (declare (indent 1))
  (cond 
   ((= 0 (length pairs))
    `(progn ,@body))
   (t 
	(let-seq (first-pair rest)
			 (split-after-two pairs)
			 `(lexical-let* ,(pairs->dlet-binding-forms (list->vector first-pair))
				(dlet ,(list->vector rest)
				  ,@body))))))


										; (dlet [[a b] (list 10 10) y 11] (+ a b y))
										; (dlet [[x y :as z] (list 1 2) b (+ x y)] (list x y b z))

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
  (let ((magnitude (car arity))
		(modifier (cadr arity)))
	(cond 
	 ((eq modifier 'exactly)
	  (= n magnitude))
	 ((eq modifier '+more)
	  (>= n magnitude))
	 ((eq modifier '-less)
	  (<= n magnitude)))))

(defun arity-comparitor (arity1 arity2)
  (let-seq (mag1 mod1) arity1
		   (let-seq (mag2 mod2) arity2
					(cond
					 ((eq mod1 mod2) (< mag1 mag2))
					 ((and
					   (or (eq mod1 'exactly) (eq 'mod1 '-less))
					   (eq mod2 '+more))
					  t)
					 ((and
					   (eq mod1 '+more)
					   (or (eq mod2 'exactly) (eq mod2 '-less)))
					  nil)
					 ((and
					   (eq mod1 'exactly)
					   (eq mod2 '+more))
					  t)
					 ((and
					   (eq mod1 'exactly)
					   (eq mod2 '-less))
					  nil)
					 ((eq mod1 '-less)
					  t)
					 ((eq mod2 '-less)
					  nil)
					 ((eq mod1 'more)
					  nil)
					 ((eq mod2 'more)
					  nil)
					 (t (error "Unknown binder comparator case %s <? %s" arity1 arity2))))))

										; (arity-comparitor '(1 +more) '(3 exactly))
										; (arity-comparitor '(3 exactly) '(1 +more))

(defun sort-arities (lst)
  (sort* lst #'arity-comparitor))

(defun random-arity ()
  (let ((mods '(exactly +more -less)))
	(list (random 20) (elt mods (random 3)))))

										; (random-arity)

										; (sort-arities (foldl (lambda (it ac) (cons (random-arity) ac)) '() '(1 2 3 4 5 6)))


										; (arity-match 2 '(3 +more))
										; (arity-match 3 '(3 -less))
										; (arity-match 2 '(2 exactly))
										; (arity-match 2 '(3 -less))

										; (handle-seq-binder [a b c & rest :or (list 10 11 12 13 14 15 16)] '(1 2 3 4 5) '())

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
							  (assert (not (member :or (coerce binders 'list))) t (format "top-level defn binding forms can't contain an or clause because it conflicts with automatic arity dispatching (%s)." currently-defining-defn))
							  `(((arity-match ,numargs ',(binder-arity binders))
								 (lexical-let* ,(mapcar 
												 (lambda (x) (coerce x 'list)) 
												 (handle-binding binders args-sym)) ,@body)))))
					  `(t (error "Unable to find an arity match for %d args in fn %s." ,numargs ',currently-defining-defn))))))))
   (t (error "Can't parse defn %s.  Defn needs a binder/body pair or a list of such pairs.  Neither appears to have been passed in. " currently-defining-defn))))

(defmacro* defn (name &rest rest)
  (declare (indent defun))
  `(let ((currently-defining-defn ',name))
	 (fset ',name (fn ,@rest))))

;(defn defn-test [] (+ 1 1))
;(binder->type [])
;(defn defn-test ([x] (+ x 1)))
(provide 'defn)

; (defn f (x x) ([a b] (+ a b) ))

; (defn a-test-f [x y [:: z :z :as a-tble :as eh]] (list (+ x y z) a-tble))
; (defn a-test-f [x y [:: z :z :as a-tble]] (list (+ x y z) a-tble))
; (a-test-f 1 2 (tbl! :z 10))
; (f 1 2 3)
; (defn f ([z a] (* z a)) (x x) )

; (defn f [z [:: a :a :as a-table :or (tbl! :a 100)]] (list z a))
; (f 10 (tbl!))

; (defn f [z [:: :keys [a b c]]] (list z (+ a b c)))
; (f 10 (tbl! :a 1 :b 2 :c 3))	

; (defn f [a b [x y z :or [1 2 3]]] (+ a b x y z))
; (f 1 2 [4])
; (defn f [a b c :or [1 2 3]] (+ a b c))

