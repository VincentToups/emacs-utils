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

; (handle-seq-binder [x y [a b :as ri] :as r] '(1 2 3) '())

;(tbl-bind-kws [:: x :x y :y :as z])

(defun assert-as-count (binder)
  (let ((as-count (count :as binder)))
	(assert 
	 (or (= 1 as-count)
		 (= 0 as-count))
	 t
	 (format "At most one ':as' allowed per binder form in %s." currently-defining-defn))
	as-count))

; (assert-as-count [a b c d ])
; (assert-as-count [a d c :as d :as f])
; (assert-as-count [a d c d :as f])

; (assert-or-count [a b c d :or x])
; (assert-or-count [a d c :or d :or f])

(defun assert-as-position (binder)
  (let* ((n (length binder))
		 (pos
		  (loop for i from 0 below (length binder)
			   and b across binder when 
			   (eq b :as) return i)))
	(assert (or
			 (= pos (- n 2))
			 (and
			  (eq (elt binder (- n 2)) :or)
			  (= pos (- n 4))))
			t
			(format ":as forms must be second to last (only followed by an :or) or last (%s)." currently-defining-defn))
	pos))

; (assert-as-position [a d c d :as f])
; (assert-as-position [a d c d :as f :or q])
; (assert-as-position [a d c d :or f :as q])

(defun assert-as-symbol (binder pos)
  (let ((v (elt binder (+ pos 1))))
	(assert (and 
			 (symbolp v)
			 (not (keywordp v)))
			t
			(format ":as binder symbol must be a symbol and not a keyowrd (%s)" currently-defining-defn))
	v))

; (assert-as-symbol [a d c d :as f] 4)
; (assert-as-symbol [a d c d :as f :or q] 4)
; (assert-as-symbol [a d c d :or f :as q] 6)
			 
(defun assert-as-ok->sym-binder (binder)
  (let ((n (assert-as-count binder)))
	(case n
	  (0 
	   t)
	  (t
	   (let* ((pos (assert-as-position binder))
			  (sym (assert-as-symbol binder pos)))
		 (list sym
			   (coerce (loop for i from 0 below (length binder)
					 and b across binder
					 when (and (not (= i pos))
							   (not (= i (+ 1 pos))))
					 collect b) 'vector)))))))

; (assert-as-ok->sym-binder [a b c :as e :or d])

(defun assert-or-count (binder)
  (let ((or-count (count :or binder)))
	(assert 
	 (or (= 1 or-count)
		 (= 0 or-count))
	 t
	 (format "At most one ':or' allowed per binder form in %s." currently-defining-defn))
	or-count))

(defun assert-or-position (binder)
  (let ((n (length binder))
		(pos
		 (loop for i from 0 below (length binder)
			   and b across binder when 
			   (eq b :or) return i)))
	(assert (or
			 (= pos (- n 2))
			 (and
			  (eq (elt binder (- n 2)) :as)
			  (= pos (- n 4))))
			t
			(format ":or forms must be second to last (only followed by an :or) or last (%s)." currently-defining-defn))
	pos))

(defun assert-or-symbol (binder pos)
  (let ((v (elt binder (+ pos 1))))
	(assert (and 
			 (symbolp v)
			 (not (keywordp v)))
			t
			(format ":or binder symbol must be a symbol and not a keyowrd (%s)" currently-defining-defn))
	v))

(defun assert-or-ok->sym-binder (binder)
  (let ((n (assert-or-count binder)))
	(case n
	  (0 
	   t)
	  (t
	   (let* ((pos (assert-or-position binder))
			  (sym (assert-or-symbol binder pos)))
		 (list sym
			   (coerce (loop for i from 0 below (length binder)
					 and b across binder
					 when (and (not (= i pos))
							   (not (= i (+ 1 pos))))
					 collect b) 'vector)))))))

; (assert-or-ok->sym-binder [a b c :as e :or d])

; (assert-as-ok [a b c :as x])

;; (defun parse-table-binder (binder) ; -> ( keys vals as or ) or throw
;;   (let ((as-count (count :as binder))
;; 		(or-count (count :or binder)))

;; (defun handle-tbl-binding2 (binder); exrp previous-lets)
;;   (let-seq (as-symbol binder) (assert-as-ok->sym-binder binder)
;;   (let-seq (or-symbol binder) (assert-or-ok->sym-binder binder)
;;     (list as-symbol or-symbol binder))))

;; (handle-tbl-binding2 [a b c :as x :or b])
	
;; (defun handle-tbl-binding (binder expr previous-lets)
;;   (let* ((as-name (get-or-produce-as-name binder))
;; 		(kws (tbl-bind-kws binder))
;; 		(syms (tbl-bind-syms binder))
;; 		(previous-lets (append previous-lets (handle-binding as-name expr))))
;; 	(append 
;; 	 previous-lets
;; 	 (loop for kw in kws
;; 		   and sym in syms
;; 		   append
;; 		   (handle-binding sym `(tbl ,as-name ,kw))))))

(defun handle-tbl-binding (binder expr previous-lets)
  (let-seq (sub-binders 
			keys
			as-sym
			or-form) (parse-and-check-tbl-binder binder)
	(let 	
		((previous-lets (append previous-lets (handle-binding as-sym expr))))
	(append 
	 previous-lets
	 (loop for kw in keys
		   and sym in sub-binders
		   append
		   (handle-binding sym `(tbl ,as-sym ,kw)))))))



; (handle-tbl-binding [:: [a b] :x y :y :as table] '(tbl 'x 10 'y 11) '())

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

; 
; (defn a-test-f [x y [:: z :z :as a-tble :as eh]] (list (+ x y z) a-tble))
; (a-test-f 1 2 (tbl! :z 10))
; (f 1 2 3)
; (defn f ([z a] (* z a)) (x x) )

	



