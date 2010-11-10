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

;; (defun table-like-get (tbl-like kw)
;;   (cond ((hash-table-p tbl-like) (tbl tbl-like kw))
;; 		((listp tbl-like) (cadr (assq kw tbl-like)))))
;; (defun* table-like-get-or (tbl-like kw &optional (or-val nil))
;;   (cond ((hash-table-p tbl-like) (tbl-or tbl-like kw or-val))
;; 		((listp tbl-like) 
;; 		 (let ((v (assoc-default kw tbl-like #'eq nil)))
;; 		   (if v (car v) or-val)))))

(dont-do
 (table-like-get (tbl! :x 10 :y 10) :x)
 (table-like-get-or (tbl! :x 10 :y 10) :z 'z)
 (table-like-get (alist>> :x 20 :y 30) :x)
 (table-like-get-or (alist>> :x 20 :y 50) :x 'z)
 (cadr (assoc-default :x '((:x 10) (:y 20)))))


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
						 (handle-binding sym `(table-like-get ,as-sym ,kw))
					   (handle-binding sym `(table-like-get-or ,as-sym ,kw (table-like-get ,or-form-name ,kw)))))))))

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

(defmacro* dlet_ (pairs &body body)
  (declare (indent 1))
  (cond 
   ((= 0 (length pairs))
	`(progn ,@body))
   (t 
	(let-seq (first-pair rest)
			 (split-after-two pairs)
			 `(let* ,(pairs->dlet-binding-forms (list->vector first-pair))
				(dlet ,(list->vector rest)
				  ,@body))))))

(defun* build-dsetq* (pairs &optional (output (list 'progn)))
  (if (= (length pairs) 0) (reverse output)
	(let-seq (first-pair rest) (split-after-two pairs)
			 (let ((forms 
					(mapcar
					 (lambda (x) (cons 'setq x))
					 (pairs->dlet-binding-forms (list->vector first-pair)))))
			   (build-dsetq* rest (append (reverse forms) output))))))

(defmacro* dsetq* (&rest pairs)
  (if (oddp (length pairs)) (error "dsetq needs an even number of elements.")
	(build-dsetq* pairs)))

(defun build-dsetq (pairs)
  (let* ((val-forms (loop for form in pairs and i from 0 when (oddp i) collect form))
		 (binders   (loop for form in pairs and i from 0 when (evenp i) collect form))
		 (names (loop for i from 0 below (length val-forms) collect
					  (gensym (format "dsetq-val-%d-" i))))
		 (regular-let-binders
		  (loop for form in val-forms and name in names collect
				`(,name ,form))))
	`(let ,regular-let-binders
	   ,@(let ((setting-statements nil))
		   (loop while binders do
				 (let ((binder (pop binders))
					   (name (pop names)))
				   (setq setting-statements 
						 (append setting-statements 
								 (mapcar (lambda (x) (cons 'setq x))
										 (pairs->dlet-binding-forms (vector binder name)))))))
		   setting-statements))))



(defmacro* dsetq (&rest pairs)
  (if (oddp (length pairs)) (error "dsetq needs an even number of elements.")
	(build-dsetq pairs)))
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



(defun gen-fn-rec-binding (binders args-sym)
  (vector (coerce binders 'vector) args-sym))

(defmacro* fnc (&rest rest)
  `(byte-compile (fn ,@rest)))
(defmacro* fnc_ (&rest rest)
  `(byte-compile (fn_ ,@rest)))

(defmacro* fn (&rest rest)
  "Clojure-style destructuring lambda (funcall (fn [[x y & r]] (list x y r)) '(1 2 3 4 5 6)) -> (1 2 (3 4 5 6))."
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
							(let* ((binders (car pair))
								   (body (cdr pair))
								   (expanded-body (macroexpand-all body))
								   (uses-recur? ($ 'recur in (flatten expanded-body)))
								   (apropriate-body 
									(if uses-recur? 
										`(dloop-single-arg
										  ,(gen-fn-rec-binding binders args-sym)
										  ,@expanded-body)
									  `(lexical-let* ,(mapcar 
													   (lambda (x) (coerce x 'list)) 
													   (handle-binding binders args-sym)) ,@body))))
							  (assert 
							   (vectorp binders) 
							   t 
							   (format "binder forms need to be vectors (error in %s)." currently-defining-defn))
							  (assert 
							   (not (member :or (coerce binders 'list))) 
							   t 
							   (format 
								"top-level defn binding forms can't contain an or clause because it conflicts with automatic arity dispatching (%s)." 
								currently-defining-defn))
							  `(((arity-match ,numargs ',(binder-arity binders))
								 ,apropriate-body))))
					  `(t 
						(error 
						 "Unable to find an arity match for %d args in fn %s." 
						 ,numargs 
						 ',currently-defining-defn))))))))
   (t 
	(error 
	 "Can't parse defn %s.  Defn needs a binder/body pair or a list of such pairs.  Neither appears to have been passed in. " 
	 currently-defining-defn))))

(defmacro* fn_ (&rest rest)
  "Clojure-style destructuring lambda (funcall (fn [[x y & r]] (list x y r)) '(1 2 3 4 5 6)) -> (1 2 (3 4 5 6)).  Non-lexical binding version."
  (cond
   ((vectorp (car rest))
	`(fn_ (,(car rest) ,@(cdr rest))))
   ((listp (car rest))	   ; set of different arity binders/bodies
	(let ((args-sym (gensym))
		  (numargs (gensym)))
	  `(lambda (&rest ,args-sym) 
		 (let ((,numargs (length ,args-sym)))
		   (cond
			,@(suffix (loop for pair in rest append
							(let* ((binders (car pair))
								   (body (cdr pair))
								   (expanded-body (macroexpand-all body))
								   (uses-recur? ($ 'recur in (flatten expanded-body)))
								   (apropriate-body 
									(if uses-recur? 
										`(dloop-single-arg_
										  ,(gen-fn-rec-binding binders args-sym)
										  ,@expanded-body)
									  `(let* ,(mapcar 
											   (lambda (x) (coerce x 'list)) 
											   (handle-binding binders args-sym)) ,@body))))
							  (assert 
							   (vectorp binders) 
							   t 
							   (format "binder forms need to be vectors (error in %s)." currently-defining-defn))
							  (assert 
							   (not (member :or (coerce binders 'list))) 
							   t 
							   (format 
								"top-level defn binding forms can't contain an or clause because it conflicts with automatic arity dispatching (%s)." 
								currently-defining-defn))
							  `(((arity-match ,numargs ',(binder-arity binders))
								 ,apropriate-body))))
					  `(t 
						(error 
						 "Unable to find an arity match for %d args in fn %s." 
						 ,numargs 
						 ',currently-defining-defn))))))))
   (t 
	(error 
	 "Can't parse defn %s.  Defn needs a binder/body pair or a list of such pairs.  Neither appears to have been passed in. " 
	 currently-defining-defn))))

(defun extract-interactive-and-return (forms)
  (loop with 
		interactives = nil
		and
		outforms = nil
		for form in forms do
		(if (and (listp form)
				 (eq (car form) 'interactive))
			(push form interactives)
		  (push form outforms))
		finally 
		(return (list (reverse interactives) (reverse outforms)))))

(defmacro defunc (&rest rest)
  (let ((retsym (gensym "defunc-val")))
	`(let ((,retsym (defun ,@rest)))
	   (byte-compile ',(car rest))
	   ,retsym)))

(defmacro* defn (name &rest rest)
  "Clojure-style function definition.  Supports recur and destructuring bind."
  (declare (indent defun))
  (let-seq (interactives clean-rest) (extract-interactive-and-return rest)
		   (if ($ (length interactives) > 1) (error "Too many interactive forms in %s." name))
		   (let ((undername (gensym (format "%s-" name)))
				 (args (gensym (format "%s-args-" name))))
			 `(let ((currently-defining-defn ',name))
				(lexical-let ((,undername (fn ,@clean-rest)))
				  (defunc ,name (&rest ,args) ,(car interactives)
					(apply ,undername ,args)))))))

(defmacro* defn_ (name &rest rest)
  "Clojure-style function definition.  Supports recur and destructuring bind.  Non-lexical binding version."
  (declare (indent defun))
  (let-seq (interactives clean-rest) (extract-interactive-and-return rest)
		   (if ($ (length interactives) > 1) (error "Too many interactive forms in %s." name))
		   (let ((undername (gensym (format "%s-" name)))
				 (args (gensym (format "%s-args-" name))))
			 `(let ((currently-defining-defn ',name))
				(lexical-let ((,undername (fn_ ,@clean-rest)))
				  (defunc ,name (&rest ,args) ,(car interactives)
					(apply ,undername ,args)))))))

										;(defn defn-test [] (+ 1 1))
										;(binder->type [])
										;(defn defn-test ([x] (+ x 1)))

(defun ifp (form) 
  (and (listp form)
	   (eq (car form) 'if)))
(defun condp (form)
  (and (listp form)
	   (eq (car form) 'cond)))
(defun casep (form)
  (and (listp form)
	   (eq (car form) 'case)))
(defun recurp (form)
  (and (listp form)
	   (eq (car form) 'recur)))
(defun prognp (form)
  (and (listp form)
	   (eq (car form) 'progn)))
(defun expand-recur-cond-pair (cond-pair parent-is-tale loop-sentinal binding-forms)
  `(,(car cond-pair)
	,@(cdr (expand-recur `(progn ,@(cdr cond-pair)) parent-is-tale loop-sentinal binding-forms))))
;; (defun expand-recur-recur (form parent-is-tale loop-sentinal binding-forms)
;;   `(progn 
;; 	 (setq ,loop-sentinal t)
;; 	 (dsetq ,@(loop for b in (coerce binding-forms 'list) and v in (cdr form) 
;; 					collect b and collect v))))
(defun expand-recur-recur (form parent-is-tale loop-sentinal binding-forms)
  `(progn 
	 (setq ,loop-sentinal t)
	 (dsetq ,@binding-forms (list ,@(cdr form)))))


(defun let-likep (form)
  (and (listp form)
	   form
	   (let ((f (car form)))
		 (or
		  (eq f 'let)
		  (eq f 'flet)
		  (eq f 'labels)
		  (eq f 'lexical-let)
		  (eq f 'lexical-let*)
		  (eq f 'let*)))))

(defun* expand-recur (form parent-is-tale loop-sentinal binding-forms &optional (single-arg-recur nil))
  (let ((mxform (macroexpand form)))
	(cond ((symbolp mxform) mxform)
		  ((numberp mxform) mxform)
		  ((stringp mxform) mxform)
		  ((arrayp  mxform) mxform)
		  ((listp mxform)
		   (case parent-is-tale
			 (nil mxform)
			 (t
			  (cond
			   ((ifp mxform)
				`(if ,(cadr mxform) ,@(mapcar
									   (lambda (x) (expand-recur x t loop-sentinal binding-forms single-arg-recur)) 
									   (cddr mxform))))
			   ((condp mxform)
				`(cond
				  ,@(mapcar 
					 (lambda (cond-pair) 
					   (expand-recur-cond-pair 
						cond-pair 
						parent-is-tale 
						loop-sentinal 
						binding-forms))
					 (cdr mxform))))
			   ((casep mxform)
				`(case ,(cadr mxform)
				   ,@(mapcar 
					  (lambda (cond-pair) 
						(expand-recur-cond-pair 
						 cond-pair 
						 parent-is-tale 
						 loop-sentinal 
						 binding-forms))
					  (cddr mxform))))
			   ((prognp mxform)
				`(,@(reverse (cdr (reverse mxform)))
				  ,(expand-recur (car (reverse mxform)) t loop-sentinal binding-forms single-arg-recur)))
			   ((let-likep mxform)
				(let* ((letish (car mxform))
					   (ll-binders (cadr mxform))
					   (body (cddr mxform))
					   (reverse-body (reverse body))
					   (all-but-last (reverse (cdr reverse-body)))
					   (last-item (car reverse-body)))
				  `(,letish 
					,ll-binders 
					,@all-but-last 
					,(expand-recur last-item t loop-sentinal binding-forms single-arg-recur))))
			   ((recurp mxform)
				(if single-arg-recur
					(expand-recur-recur `(recur (list ,@(cdr mxform)))
											parent-is-tale loop-sentinal binding-forms)
				(expand-recur-recur mxform parent-is-tale loop-sentinal binding-forms)))
			   (t (progn
					(if (> (length (filter (lambda (x) (and (symbolp x) (eq 'recur x))) (flatten mxform))) 0)
						(error (format "Can't recur from a non-tail position in %s" mxform)))
					mxform)))))))))

(defmacro* dloop-single-arg (bindings &body body)
  (let ((loop-sentinal (gensym "loop-sentinal"))
		(return-value (gensym "return-value"))
		(binding-parts (loop for el in (coerce bindings 'list) and i from 0
							 when (evenp i) collect el)))
	`(let ((,loop-sentinal t)
		   (,return-value nil))
	   (dlet ,bindings
		 (loop while ,loop-sentinal do
			   (setq ,return-value (progn
									 (setq ,loop-sentinal nil)
									 ,(expand-recur `(progn ,@body) t loop-sentinal binding-parts ))))
		 ,return-value))))

(defmacro* dloop-single-arg_ (bindings &body body)
  (let ((loop-sentinal (gensym "loop-sentinal"))
		(return-value (gensym "return-value"))
		(binding-parts (loop for el in (coerce bindings 'list) and i from 0
							 when (evenp i) collect el)))
	`(let ((,loop-sentinal t)
		   (,return-value nil))
	   (dlet_ ,bindings
		 (loop while ,loop-sentinal do
			   (setq ,return-value (progn
									 (setq ,loop-sentinal nil)
									 ,(expand-recur `(progn ,@body) t loop-sentinal binding-parts ))))
		 ,return-value))))


(defmacro* dloop (bindings &body body)
  (let ((loop-sentinal (gensym "loop-sentinal"))
		(return-value (gensym "return-value"))
		(binding-parts (loop for el in (coerce bindings 'list) and i from 0
							 when (evenp i) collect el)))
	`(let ((,loop-sentinal t)
		   (,return-value nil))
	   (dlet ,bindings
		 (loop while ,loop-sentinal do
			   (setq ,return-value (progn
									 (setq ,loop-sentinal nil)
									 ,(expand-recur `(progn ,@body) t loop-sentinal binding-parts))))
		 ,return-value))))

(defmacro* dloop_ (bindings &body body)
  (let ((loop-sentinal (gensym "loop-sentinal"))
		(return-value (gensym "return-value"))
		(binding-parts (loop for el in (coerce bindings 'list) and i from 0
							 when (evenp i) collect el)))
	`(let ((,loop-sentinal t)
		   (,return-value nil))
	   (dlet_ ,bindings
		 (loop while ,loop-sentinal do
			   (setq ,return-value (progn
									 (setq ,loop-sentinal nil)
									 ,(expand-recur `(progn ,@body) t loop-sentinal binding-parts))))
		 ,return-value))))



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

