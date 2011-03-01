(require 'utils)
(require 'functional)
(require 'parse-lambda-list)
(require 'cl)
(provide 'lambda-star)

(defun car-or-thing (item)
  "If ITEM Is a list, return the car, otherwise return the thing itself."
  (if (listp item)
	  (car item)
	item))
(defun cadr-or-nil (item)
  "If ITEM Is a list, return the cadr, otherwise return nil."
  (if (listp item)
	  (cadr item)
	nil))

(defun gen-optional-fill-forms (syms expr)
  "Generate forms from SYMS and EXPR for a PSETQ form to set up OPTIONAL ARGS from LAMBDA*"
  (loop for sym in syms and exp in expr
		when exp
		append 
		(list sym `(if ,sym ,sym ,exp))))

(defun gen-key-fill-forms (syms expr alist-name)
  "Generate forms from SYMS and EXPR for fill a PSETQ form for keyword args.  Use an alist called ALIST-NAME."
  (loop for sym in syms and exp in expr
		append 
		(with-gensyms 
		 ((temp sym))
		 (list sym
			   `(let ((,temp (alist ,alist-name ,(symbol->keyword sym))))
				  (if ,temp ,temp ,exp))))))

(defun optional-and-key->standard-alist&bind-splice (argalist)
  "Generate an arg list and a splice of setter code for a optional and key case bind from LAMBDA*"
  (let* ((normal (alist argalist :normal))
		 (optional (alist argalist :optional))
		 (key-part (alist argalist :key))
		 (opt-syms (mapcar #'car-or-thing optional))
		 (opt-expr (mapcar #'cadr-or-nil optional))
		 (rest-name (gensym "&key-rest-"))
		 (alist-name (gensym "&key-alist-"))
		 (key-syms (mapcar #'car-or-thing key-part))
		 (key-expr (mapcar #'cadr-or-nil key-part)))
	(print key-syms)
	(list `(,@normal &optional ,@opt-syms &rest ,rest-name)
		  (list
		   `(setq ,alist-name (apply #'alist>> ,rest-name))
		   `(psetq ,@(gen-optional-fill-forms opt-syms opt-expr)
				   ,@(gen-key-fill-forms key-syms key-expr alist-name))))))

(dont-do
 (optional-and-key->standard-alist&bind-splice (alist>> :normal '(a b) :key '(d)))
)

 (defun key-arg-symbols (argalist)
   "Return the symbols associated with key arguments in ARGALIST, a parsed lambda list."
   (mapcar #'car-or-thing 
		   (alist argalist :key)))

 (defun lambda*-optional-key-case (argalist body)
   "Build a lambda form for a case with OPTIONAL and KEYWORD args."
   (let-seq (arglist fill-splice) (optional-and-key->standard-alist&bind-splice argalist)
			`(lambda ,arglist 
			   (let ,(key-arg-symbols argalist)
				 ,@fill-splice ,@body))))

 (defun lambda*-optional-rest-case (argalist body)
   "Build a lambda form for a case with optional and rest forms."
   (let* ((normal (alist argalist :normal))
		  (rest-name (alist argalist :rest))
		  (optional (alist argalist :optional))
		  (opt-syms (mapcar #'car-or-thing optional))
		  (opt-expr (mapcar #'cadr-or-nil   optional)))
	 `(lambda (,@normal &optional ,@opt-syms ,@(if rest-name '(&rest) nil) ,@(if rest-name (list rest-name) nil))
		(psetq ,@(gen-optional-fill-forms opt-syms opt-expr))
		,@body)))

 (defmacro* lambda* (arglist &body body)
   "Lambda but with Common Lisp lambda-list semantics."
   (let ((argalist (parse-lambda-list arglist)))
	 (cond ((and
			(not (alist argalist :optional))
			(not (alist argalist :key)))
		   `(lambda ,arglist ,@body))
		  ((not (alist argalist :rest))
		   (lambda*-optional-key-case argalist body))
		  ((not (alist argalist :key))
		   (lambda*-optional-rest-case argalist body)))))
