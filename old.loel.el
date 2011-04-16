(defvar *var-tag-table* 
  (make-hash-table :weakness 'key-and-value)
  "var-tag-table is a weak table which helps identifty loel variables.")

(defun* new-var-tag (var-sym)
  (let-if tag (gethash var-sym *var-tag-table*) tag
		  (let ((tag (gensym "")))
			(puthash var-sym tag *var-tag-table*)
			tag)))

(defun tag-in-table? (sym tag)
  (eq (gethash sym *var-tag-table* nil)
	  tag))

(defun f-var (sym)
  (vector '--var (new-var-tag sym) sym))

(defun var-tag (var)
  (elt var 1))

(defun var? (o)
  (and (vectorp o)
	   (= (length o) 3)
	   (eq (elt o 0) '--var)
	   (tag-in-table? (var-sym o) (var-tag o))))

(defun var-sym (var)
  (elt var 2))

(defvar *symbol-counter* 0)
(defun new-symbol ()
  (prog1 (internf "_%d"  *symbol-counter*)
	(setq *symbol-counter* (+ 1 *symbol-counter*))))

(defmacro* var (&optional (symbol (new-symbol)))
  (if (not (symbolp symbol))
	  (error "Var must be initialized with a symbol.")
	`(f-var ',symbol)))

(example 
										; vars are specially tagged vectors which are distinct from any other
										; vector except that vars with the same symbol name are structurally
										; identical.

 (eq (var x) (var x))
 (equal (var x) (var x))
										; this appears to be the implied semantics of The Reasoned Schemer
										; with the additional proviso that one can handle arbitrarily similar
										; vector structures as long as they haven't been produced with a call
										; to f-var.

 (var? (var x)) ; detected as a var
 (var? (vector '--var 'sham-tag 'x)) ; similar vector structures are
										; rejected.  this allows the use
										; of vectors as things in the loel
										; sub-language.
 )

(defun rhs (assoc) (cdr assoc))
(defun lhs (assoc) (car assoc))

(recur-defun* walk-step  (var sub)
  (cond
   ((empty? sub) nil)
   (t
	(if (eq (var-sym var)
			(var-sym (car (car sub))))
		(car sub)
	  (recur var (cdr sub))))))

(recur-defun* walk (var sub)
  (cond
   ((var? var)
	(let ((a (walk-step var sub)))
	  (cond 
	   (a (recur (rhs a) sub))
	   (t var))))
   (t var)))

(recur-defun* walk* (var sub)
  (let ((v (walk var sub)))
	(cond
	 ((var? v) v)
	 ((pair? v)
	  (cons 
	   (walk* (car v) sub)
	   (walk* (cdr v) sub)))
	 (t v))))


(defun ext-s (lhs rhs sub)
  (cons (cons lhs rhs) sub))

(defun pair? (o)
  (if o (listp o) o))

(example


										;
 (walk (var x) (substitution (var x) 'b))
 (walk* (var v) (substitution (var x) 'b 
							  (var v) (var w)
							  (var w) (list (var x) 'c))))

(defun reify-name (n)
  (internf "_%d" n))

(defun size-s (n)
  (length s))

(defun* reify-s (v &optional (s nil))
  (let ((v (walk v s)))
	(cond
	 ((var? v)
	  (ext-s v (reify-name (size-s s)) s))
	 ((pair? v)
	  (reify-s (cdr v)
			   (reify-s (car v) s)))
	 (t s))))

(example 

 (pp (let* ((s (substitution (var x) (var y) 
							 (var v) (var x) 
							 (var y) (var q)))
			(wv (walk (var v) s)))
   (reify-s wv)))

)

(recur-defun* unify (v w sub)
  (cond 
   ((eq v w) sub)
   ((var? v) (ext-s v w sub))
   ((var? w) (ext-s w v sub))
   ((and (pair? v)
		 (pair? w))
	(let ((u (unify 
			  (lhs v)
			  (lhs w) sub)))
	  (cond
	   (u (recur (rhs v) (rhs w) u))
	   (t nil))))
   ((equal v w) sub)
   (t nil)))

(defmacro substitution (&rest pairs)
  `(list ,@(loop for pair in (bunch-list pairs) collect
				 `(cons ,(car pair) ,(cadr pair)))))

(unify (var v) (var x) (substitution (var v) 10 (var z) (var x)))