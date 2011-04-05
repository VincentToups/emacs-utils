(provide 'loel)
(require 'cl)
(require 'recur)

(defvar *var-tag-table* 
  (make-hash-table :weakness 'key)
  "var-tag-table is a weak table which helps identifty loel variables.")

(defun* new-var-tag (&optional (prefix ""))
  (let ((sym (gensym prefix)))
	(puthash sym t *var-tag-table*)
	sym))

(defun tag-in-table? (tag)
  (gethash tag *var-tag-table* nil))

(defun f-var (sym)
  (vector '--var (new-var-tag) sym))

(defun var? (o)
  (and (vectorp o)
	   (= (length o) 3)
	   (eq (elt o 0) '--var)
	   (tag-in-table? (elt o 1))))

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

(defun rhs (assoc) (cdr assoc))
(defun lhs (assoc) (car assoc))

(recur-defun* 
 walk-step 
 (var sub)
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


   (defmacro substitution (&rest pairs)
	 `(list ,@(loop for pair in (bunch-list pairs) collect
					`(cons ,(car pair) ,(cadr pair)))))

