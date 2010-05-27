(require 'utils)

(defun non-empty-listp (o)
  (and (listp o)
	   (> (length o) 0)))

(defun quotep (o)
  (and (listp o)
	   (> (length o) 0)
	   (eq (car o) 'quote)))
(setq *setter-symbols*
	  '(setq setf))
(defun setterp (o)
  (and (listp o)
	   (> (length o) 1)
	   (let ((type (first o)))
		 (foldl 
		  (lambda (it ac)
			(or ac (eq o it)))
		  nil
		  *setter-symbols*))))

(defun get-setter-symbols (form)
  (let-repeatedly _ (cdr form)
				  (loop for f in _ and
						i from 1 
						when (oddp i) collect f)))
(defun get-setter-expressions (form)
  (let-repeatedly _ (cdr form)
				  (loop for f in _ and 
						i from 1 
						when (evenp i) collect f)))

(defun setter-binds-symbolp (sym form)
  ($ sym in (get-setter-symbols form)))

(dont-do
 (get-setter-symbols '(setq x 10 y 11 z 12))
 (get-setter-expressions '(setq x 10 y 11 z 12))
 )

(setq *letter-symbols*
	  '(let let* lexical-let lexical-let*))

(defun prognp (form)
  (and (non-empty-listp form)
	   (eq (car form) 'progn)))

(defun prog1p (form)
  (and (non-empty-listp form)
	   (eq (car form) 'prog1)))

(defun letp (o)
  (and (non-empty-listp o)
	   (eq (car o) 'let)))
(defun let*p (o)
  (and (non-empty-listp o)
	   (eq (car o) 'let*)))


(defun let-likep (o)
  (and (non-empty-listp o)
	   (let ((type (car o)))
		 (foldl 
		  (lambda (it ac)
			(or ac 
				(eq it type)))
		  nil 
		  *letter-symbols*))))

(defun let-form-binds-symbolp (sym form)
  ($ sym in (mapcar #'car (second form))))

(defun get-let-body (let-form) 
  (cddr let-form))
(defun get-let-binders (let-form)
  (cadr let-form))

(dont-do 
 (get-let-body '(let ((a b) (c d)) (+ a b) (+ c d))))

(defun lambdap (o)
  (and (non-empty-listp o)
	   (eq (car o) 'lambda)))

(defun lambda-list-optionals (llist)
  (let-repeatedly _ (member-if (lambda (x) (eq '&optional x))
							   llist)
				  (if _ (cdr _) _)))
(defun lambda-list-rest (llist)
  (let-repeatedly _ (member-if (lambda (x) (eq '&rest x))
							   llist)
				  (if _ (cdr _) _)))
(defun lambda-list-normal-args (llist)
  (let-repeatedly _ 
				  (split-list-right 
				   llist 
				   (lambda (x)
					 (or
					  (eq x '&rest)
					  (eq x '&optional))))
				  (first _)))

(defun lambda-form-binds-symbolp (sym form)
  (let ((llist (cadr form)))
	($ sym in
	   (filter (lambda (x) (not (or (eq x '&optional)
									(eq x '&rest))))
			   llist))))

(defun get-lambda-body (form)
  (cddr form))


(dont-do 
 (lambda-form-binds-symbolp 'x (lambda (x y &rest z) (+ x y)))
 (funcall (lambda (x &optional y) (list x y ))
		  1 2)
 (get-lambda-body '(lambda (x y z) (+ a b c) (+ q r)))
 )

(setq *defunners* '(defun defun*))
(defun defun-likep (o)
  (and (non-empty-listp o)
	   ($ (car o) in *defunners* #'eq)))


(defun third-on (lst)
  (nthcdr 2 lst))

(defun* count-free-usages-let (sym form &optional (n 0))
  (let ((n-in-binders 
		 (apply #'+
				(mapcar 
				 (lambda (x) (count-free-usages sym (cadr x) 0))
				 (get-let-binders form))))
		(n-in-body (count-free-usages sym `(progn ,@(get-let-body form)) 0)))
	(+ n-in-body n-in-binders)))

(defun* count-free-usages-let* (sym form &optional (n 0))
  (let* ((binders-before-binding-sym
		  (car (split-list-left  (get-let-binders form) (lambda (br) (eq (car br) sym)))))
		 (n-in-unbound-binders 
		  (apply #'+
				 (mapcar 
				  (lambda (x) (count-free-usages sym (cadr x) 0))
				  binders-before-binding-sym))))
	(if (= (length (get-let-binders form))
		   (length binders-before-binding-sym))
		(+ n-in-unbound-binders (count-free-usages sym `(progn ,@(get-let-body form)) 0))
	  n-in-unbound-binders)))

(defun* count-free-usages-let-like (sym form &optional (n 0))
  (cond 
   ((letp form) (count-free-usages-let sym form n))
   ((let*p form) (count-free-usages-let* sym form n))))

(defun* count-free-usages (sym form &optional (n 0))
  (cond
   ((symbolp form)
	(if (eq form sym) (+ n 1) n))
   ((numberp form) n)
   ((stringp form) n)
   ((listp form)
	(cond
	 ((or (prog1p form) (prognp form))
	  (apply #'+ (cons n (mapcar 
						  (lambda (sform) (count-free-usages sym sform)) 
						  (cdr form)))))
	 ((let-likep form)
	  (count-free-usages-let-like sym form n))
	 ((lambdap form)
	  (if (lambda-form-binds-symbolp sym form) n
		(apply #'+ (cons 
					n
					(mapcar 
					 (lambda (sform)
					   (count-free-usages sym sform))
					 (get-lambda-body form))))))
	 ((setterp form)
	  (apply #'+
			 (cons n 
				   (mapcar
					(lambda (sform)
					  (count-free-usages sym sform))
					(get-setter-expressions form)))))
	 (t (apply #'+ (cons n (mapcar (lambda (subform) 
							 (count-free-usages sym subform 0))
						   (cdr form)))))))))

(dont-do 
 (count-free-usages 'x '(let* ((z (+ x 1)) (x (+ x 10)) (y (+ x 10)) (x 10)) (+ x x)))
 (count-free-usages 'x '(let ((a b)) x x (lambda (x) (+ x x)))))

(defun print-and-return (f)
  (print f)
  f)

(defun optimize-let-form-once (form)
  (let ((type-of-let (first form))
		(bindings (second form))
		(body (third-on form)))
	`(,type-of-let ,(loop 
	 with new-bindings = nil
	 for binding in bindings 
	 and i from 1
	 do
	 (let* ((sym (car binding))
			(val-form (cadr binding))
			(subsequent-bindings 
			 (nthcdr i bindings))
			(n-uses-in-sub-bindings 
			 (count-free-usages sym `(,type-of-let ,subsequent-bindings ,@body))))
	   (if (> n-uses-in-sub-bindings 0) (push binding new-bindings)))
	 finally (return (reverse new-bindings))) ,@body)))

