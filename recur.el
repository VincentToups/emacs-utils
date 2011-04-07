(require 'macro-utils)
(require 'utils)
(require 'functional)
(require 'parse-lambda-list)
(require 'lambda-star)
(require 'cl)
(provide 'recur)


(defun simple-expand-recur-progn (code symbols in-tail loop-sentinal)
  "Handle expansion of tail recursion for a PROGN form."
  (let* ((r-code (reverse (cdr code)))
		 (tail (car r-code))
		 (rest (cdr r-code)))
	`(progn ,@(reverse (cons (simple-expand-recur tail symbols in-tail loop-sentinal)
							 (mapcar (par #'simple-expand-recur symbols nil nil) rest))))))

(defun simple-recurp (form)
  "Test to see if this is a recur form."
  (and (listp form)
	   (eq (car form) 'recur)))

(defun simple-expand-recur-recur (code symbols in-tail loop-sentinal)
  "Expand a RECUR form.  Might be useful to shadow for special kinds of recursion bindings."
  (if (not in-tail) (error "The recur form %S is not in a tail position, can't expand." code))
  (let* ((val-exprs (cdr code))
		 (psetq-forms (flatten-once (zip (cons loop-sentinal symbols) (cons t val-exprs)))))
	`(psetq ,@psetq-forms)))



(defun simple-expand-recur-let-like (form symbols in-tail loop-sentinal)
  "Handle recursion expansion for LET forms."
  (let* ((body (cdr (simple-expand-recur `(progn ,@(get-let-body form)) symbols in-tail loop-sentinal)))
		 (bindings (get-let-binders form))
		 (symbols (mapcar #'car bindings))
		 (expressions (mapcar 
 					   (comp (par #'simple-expand-recur nil nil nil) #'cadr) 
 					   bindings))
		 (bindings (zip symbols expressions)))
	`(let ,bindings ,@body)))

(defun simple-expand-recur-if (form symbols in-tail loop-sentinal)
  "Handle recursion expansion for IF forms."
  (let* ((predicate-expr (simple-expand-recur (elt form 1) nil nil nil))
		 (true-branch 
		  (simple-expand-recur (elt form 2) symbols in-tail loop-sentinal))
		 (false-branch 
		  (simple-expand-recur (elt form 3) symbols in-tail loop-sentinal)))
	`(if ,predicate-expr ,true-branch ,false-branch)))

(defun simple-expand-recur-cond (form symbols in-tail loop-sentinal)
  "Handle recursion expansion for COND forms."
  `(cond ,@(loop for sub-form in (cdr form)
				 collect
				 (let ((condition (car sub-form))
					   (body (cdr sub-form)))
				   `(
					 ,(simple-expand-recur condition symbols nil nil)
					 ,@(cdr (simple-expand-recur
							 `(progn ,@body) symbols in-tail loop-sentinal)))))))

(defun simple-expand-recur-funcall (code symbols in-tail loop-sentinal)
  "Handle recursion expansion for FUNCALL forms.  The de-factor default when the head of a list is not recognized."
  (let ((f (car code))
		(args (mapcar (par #'simple-expand-recur symbols nil loop-sentinal) (cdr code))))
	`(,f ,@args)))

(defun simple-expand-recur-flet (code symbols in-tail loop-sentinal)
  "Handle recursion expansion for FLET forms."
  (let ((bindings (elt code 1))
		(body (cddr code)))
	`(flet ,bindings
	   ,@(cdr (simple-expand-recur (cons 'progn body) symbols in-tail loop-sentinal)))))

(defun lambdap (code)
  "Returns true if CODE is a list starting with LAMBDA"
  (and (listp code)
	   (eq (car code) 'lambda)))


(defun defunp (code)
  "Returns true if CODE is a list starting with DEFUN."
  (and (listp code)
	   (eq (car code) 'defun)))

(defun function-listp (code)
  "Returns true if CODE is a list starting with FUNCTION"
  (and (listp code)
	   (eq (car code) 'function)))

(defun recur-ignorable-p (code)
  "Returns true for code which recur does not expand."
  (or (lambdap code)
	  (defunp code)
	  (function-listp code)))

(defun* simple-expand-recur (code symbols &optional (in-tail t) (loop-sentinal (gensym "recur-loop-sentinal-")))
  "Work horse of recur-enabled forms.  Recursively walks CODE, finding RECUR forms and 
expanding them if they are in tail position or marking an error if they are not in tail
position and marking an error otherwise.  When a recur form is found in tail position, 
it generates the appropriate setters and sets the loop-sentinal symbol to t, ensuring 
a loop continues. Building this loop is handled by the caller."
  (cond 
   ((or (atom code) (recur-ignorable-p code)) code)
   ((quotep code) code)
   ((functionp code) code)
   ((prog-like code) 
	(simple-expand-recur-progn code symbols in-tail loop-sentinal))
   ((let/*p code) 
	(simple-expand-recur-let-like code symbols in-tail loop-sentinal))
   ((ifp code)
	(simple-expand-recur-if code symbols in-tail loop-sentinal))
   ((condp code)
	(simple-expand-recur-cond code symbols in-tail loop-sentinal))
   ((fletp code)
	(simple-expand-recur-flet code symbols in-tail loop-sentinal))
   ((simple-recurp code)
	(simple-expand-recur-recur code symbols in-tail loop-sentinal))
   ((listp code)
	(simple-expand-recur-funcall code symbols nil loop-sentinal))))

(dont-do
 (simple-expand-recur-recur '(recur (+ x 1) (+ y 2)) '(r s) t)
 (simple-expand-recur '(progn a b c (recur a b))
					  '(x y) t)
 (simple-expand-recur '(let ((x 10) (y 11)) (recur (+ x 1) y)) '(q r) t 'loop-sent)
 (simple-expand-recur '(if (< x 10) (recur (+ x 1) x) (recur (+ z 2) x)) '(q r) t 'loop-sent)
 (simple-expand-recur-cond '(cond ((< x 1) a b (recur (+ x 1)))
								  ((= x 0) q  r (recur (- x 1))))
						   '(z) t 'loop-sent)
 )


(defmacro lambda-list-parsing-lambda (lambda-list)
  "Builds a lambda which turns its arguments into a table reflecting the LAMBDA-LIST.  Useful for 
macro expansion."
  (let* ((arg-alist (parse-lambda-list lambda-list))
		 (normal-names (alist arg-alist :normal))
		 (normal-names-forms 
		  (mapcar (lambda (x)
					`(list (quote ,x) ,x)) normal-names))
		 (optional-names (mapcar #'car-or-thing (alist arg-alist :optional)))
		 (optional-names-forms 
		  (mapcar (lambda (x)
					`(list (quote ,x) ,x)) optional-names))
		 (key-names (mapcar #'car-or-thing (alist arg-alist :key)))
		 (key-names-forms 
		  (mapcar (lambda (x)
					`(list (quote ,x) ,x)) key-names))
		 (rest-name (alist arg-alist :rest))
		 (rest-name-form `(list (quote ,rest-name) ,rest-name)))
	`(lambda* ,lambda-list
			  (alist>> :normal (list ,@normal-names-forms)
					   :optional (list ,@optional-names-forms)
					   :rest ,rest-name-form
					   :key (list ,@key-names-forms)))))

(defun setq-ll-normal-part (table)
  "Build the normal argument part of a lambda-list table."
  (flatten-once (alist table :normal)))

(defun setq-ll-optional-part (table)
  "Build the optional argument part of a lambda-list table."
  (flatten-once (alist table :optional)))

(defun print-and-return (x)
  "Print and return a value."
  (print x)
  x)

(defun setq-ll-key-part (table)
  "Build the key part of a lambda-list table."
  (flatten-once (alist table :key)))

(defun setq-ll-rest-part (table)
  "Build a rest part of a lambda list table."
  (let* ((rest (alist table :rest))
		 (name (car rest))
		 (forms (cadr rest)))
	(if name
		`(,name (list ,@forms)) nil)))

(defmacro setq-lambda-list (lambda-list &rest args)
  "Set variables with specifications from a lambda-list (common-lisp-style)."
  (if (not (listp lambda-list)) (error "lambda-list must be a static list conforming to the lambda lisp specifier."))
  (let* ((parser (eval `(lambda-list-parsing-lambda ,lambda-list)))
		 (table (apply parser args)))
	`(psetq ,@(setq-ll-normal-part table) ,@(setq-ll-optional-part table)
			,@(setq-ll-key-part table) ,@(setq-ll-rest-part table))))

(defmacro recur-let (bindings &rest body)
  "Like let, but allows recursion, as if the let form was itself a function which can be called from inside itself."
  (let* ((loop-sentinal (gensym "recur-loop-sentinal-"))
		 (symbols (mapcar #'car bindings))
		 (return-value (gensym "recur-loop-return-value-"))
		 (expressions (mapcar #'cdr bindings)))
	`(let ((,loop-sentinal t)
		   (,return-value nil))
	   (let ,bindings
		 (while ,loop-sentinal 
		   (setq ,loop-sentinal nil)
		   (setq ,return-value 
				 ,(simple-expand-recur 
				   (macroexpand-all 
					(cons 'progn body)) 
				   symbols 
				   t 
				   loop-sentinal))))
	   ,return-value)))

(defun simple-expand-recur-recur-lambda-list (code in-tail loop-sentinal lambda-list)
  "Special recur form expansion function for recur-defun* to support setq with lamba-list bindings."
  (if (not in-tail) (error "The recur form %S is not in a tail position, can't expand." code))
  (let* ((val-exprs (cdr code)))
	`(progn
	   (setq ,loop-sentinal t)
	   ,(macroexpand-all `(setq-lambda-list ,lambda-list ,@val-exprs)))))

(dont-do
 (macroexpand-all (simple-expand-recur-recur-lambda-list '(recur 1 (+ 2 b) 3) t 'loop-sent '(a b &rest c))))

(defmacro* recur-defun* (name arglist &body body)
  "Define a recur-enabled function.  It can call itself with a RECUR form without growing the stack.
Otherwise conforms to a Common-Lisp style defun form."
  (declare (indent defun))
  (let* ((doc (if (stringp (car body)) (car body) ""))
		 (body (if (stringp (car body)) (cdr body) body)))
	(with-gensyms 
	 (loop-sentinal return-value)
	 (lexical-let ((recur-defun-arglist arglist))
	   (let ((expanded-body (macroexpand-all body)))
		 `(defun* ,name ,arglist 
			,doc
			(let ((,loop-sentinal t)
				  (,return-value nil))
			  (while ,loop-sentinal 
				(setq ,loop-sentinal nil)
				(setq ,return-value 
					  ,(flet
						   ((simple-expand-recur-recur (code symbols in-tail loop-sentinal)
													   (simple-expand-recur-recur-lambda-list code in-tail loop-sentinal recur-defun-arglist)))
						 (simple-expand-recur 
						  (macroexpand-all 
						   (cons 'progn body)) 
						  nil 
						  t 
						  loop-sentinal))))
			  ,return-value)))))))
(dont-do 
 (recur-defun* eleven (&optional (x 0)) "counts to eleven" (if (< x 11) (recur (+ x 1)) x)))

