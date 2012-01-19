
;;;; shadchen.lisp

;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(defstruct match-fail-struct)

(defvar *match-fail* (make-match-fail-struct))

(defun non-keyword-symbol (o)
  (and (symbolp o)
	   (not (keywordp o))))

(defun match-list-expander* (sub-expressions match-value body)
  (cond 
   ((not sub-expressions) `(if (not ,match-value) (progn ,@body) *match-fail*))
   (:otherwise
	(let ((first-expression (car sub-expressions))
		  (list-name (gensym "MATCH-LIST-EXPANDER*-")))
	  `(let ((,list-name ,match-value))
		 (if (and (listp ,list-name)
				  ,list-name)
			 (match1 ,first-expression (car ,list-name)
					 (match1 (list ,@(cdr sub-expressions)) (cdr ,list-name) 
							 ,@body))
		   *match-fail*))))))

(defun match-list-expander (match-expression match-value body)
  (match-list-expander* (cdr match-expression) match-value body))


(defun match-cons-expander (match-expression match-value body)
  (let ((car-match (elt match-expression 1))
		(cdr-match (elt match-expression 2))
		(name (gensym "MATCH-CONS-EXPANDER-")))
	`(let ((,name ,match-value))
	   (if (listp ,name)
		   (match1 ,car-match (car ,name)
				   (match1 ,cdr-match (cdr ,name)
						   ,@body))))))

(defun match-quote-expander (match-expression match-value body)
  `(if (equalp ,match-expression ,match-value) (progn ,@body) *match-fail*))

(defun match-backquote-expander (match-expression match-value body)
  (let ((datum (cadr match-expression)))
	(cond 
	 ((not datum) `(progn ,@body))
	 ((and (listp datum)
		   (eq (car datum) 'uq))
	  (let ((sub-match (cadr datum)))
		`(match1 ,sub-match ,match-value ,@body)))
	 ((listp datum)
	  (let ((first-qt (car datum))
			(rest-bq (cdr datum))
			(name (gensym "MATCH-BACKQUOTE-EXPANDER-")))
		`(let ((,name ,match-value))
		   (if (and ,name
					(listp ,name))
			   (match1 (bq ,first-qt) (car ,name)
					   (match1 (bq ,rest-bq) (cdr ,name) ,@body))
			 *match-fail*))))
	 (:otherwise 
	  `(match1 ',datum ,match-value ,@body)))))

(defun match-and-expander* (sub-expressions match-name body)
  (cond 
   ((not sub-expressions) `(progn ,@body))
   (:otherwise 
	(let ((s1 (car sub-expressions))
		  (name (gensym "MATCH-AND-EXPANDER*-")))
	  `(match1 ,s1 ,match-name 
			   (match1 (and ,@(cdr sub-expressions)) ,match-name ,@body))))))

(defun match-and-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-AND-EXPANDER-")))
	`(let ((,name ,match-value))
	   ,(match-and-expander* (cdr match-expression) name body))))

(defun match-?-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-?-EXPANDER-NAME-"))
		(f-name (gensym "MATCH-?-EXPANDER-FUNCTION-")))
	(case (length (cdr match-expression))
	  (0 (error "MATCH1: MATCH-?-EXPANDER: zero arguments to MATCH-?-EXPANDER.  Needs 1 or 2."))
	  (1 `(let ((,name ,match-value)
				(,f-name ,(cadr match-expression)))
			(if (funcall ,f-name ,name) (progn ,@body) *match-fail*)))
	  (2 `(let ((,name ,match-value)
				(,f-name ,(cadr match-expression)))
			(if (funcall ,f-name ,name) (match1 ,(elt match-expression 2) ,name ,@body)
			  *match-fail*)))
	  (otherwise
	   (error "MATCH-?-EXPANDER: MATCH-?-EXPANDER takes only 1 or 2 arguments.")))))

(defun match-values-expander (match-expression match-value body)
  (let ((name (gensym "MATCH-VALUES-EXPANDER-")))
	`(let ((,name (multiple-value-list ,match-value)))
	   (match1 (list ,@(cdr match-expression)) ,name ,@body))))

(defun match-funcall-expander (match-expression match-value body)
  (assert (and (listp match-expression) (= 3 (length match-expression)))
		  (match-expression)
		  "MATCH-FUNCALL-EXPANDER: FUNCALL match expression must have
two terms, a function and a match against the result.  Got
%s." match-expression)
  (let ((name (gensym "MATCH-FUNCALL-EXPANDER-NAME-"))
		(fun-name (gensym "MATCH-FUNCALL-EXPANDER-FUN-NAME-"))
		(result-name (gensym "MATCH-FUNCALL-EXPANDER-RESULT-NAME-")))
	`(let* ((,name ,match-value)
			(,fun-name ,(cadr match-expression))
			(,result-name (funcall ,fun-name ,name)))
	   (match1 ,(caddr match-expression) ,result-name ,@body))))

(defvar *extended-patterns* (make-hash-table) "Holds user declared patterns.")
(defun extended-patternp (pattern-head) 
  "Return T if PATTERN-HEAD indicates a user provided pattern."
  (multiple-value-bind (val in) (gethash pattern-head *extended-patterns*)
	in))

(defun match-extended-pattern-expander (match-expression match-value body)
  (let* ((pattern-args (cdr match-expression))
		 (pattern-fun (gethash (car match-expression) *extended-patterns*))
		 (expansion (apply pattern-fun pattern-args)))
	`(match1 ,expansion ,match-value ,@body)))

(defmacro* defpattern (name args &body body)
  `(setf (gethash ',name *extended-patterns*)
		 #'(lambda ,args ,@body)))

(defun match-literal-string (match-expression match-value body)
  `(if (string= ,match-expression ,match-value) 
	   (progn ,@body)
	   *match-fail*))

(defun match-literal-number (match-expression match-value body)
  `(if (= ,match-expression ,match-value)
	   (progn ,@body)
	   *match-fail*))

(defun match-literal-keyword (match-expression match-value body)
  `(if (eq ,match-expression ,match-value)
	   (progn ,@body)
	   *match-fail*))


(defmacro* match1 (match-expression match-value &body body)
  (cond 
   ((non-keyword-symbol match-expression)
	`(let ((,match-expression ,match-value))
	   ,@body))
   ((stringp match-expression) 
	(match-literal-string match-expression match-value body))
   ((numberp match-expression)
	(match-literal-number match-expression match-value body))
   ((keywordp match-expression)
	(match-literal-keyword match-expression match-value body))
   ((extended-patternp (car match-expression)) 
	(match-extended-pattern-expander match-expression match-value body))
   ((listp match-expression)
	(case (car match-expression)
	  (list (match-list-expander match-expression match-value body))
	  (cons (match-cons-expander match-expression match-value body))
	  (quote (match-quote-expander match-expression match-value body))
	  (and (match-and-expander match-expression match-value body))
	  (? (match-?-expander match-expression match-value body))
	  (funcall (match-funcall-expander match-expression match-value body))
	  (bq (match-backquote-expander match-expression match-value body))
	  (values (match-values-expander match-expression match-value body))))
   (:otherwise (error "MATCH1: Unrecognized match expression: %s." match-expression))))

(defmacro* match-helper (value &body forms)
  (assert (symbolp value)
		  (value)
		  "MATCH-HELPER: VALUE must be a symbol!  Got %s." value)
  (cond 
   ((not forms) `(error "No Match for %s!" ,value))
   ((listp forms)
	(let ((first-form (car forms)))
	  (assert (and (listp first-form)
				   (> (length first-form) 1))
			  (first-form)
			  "Each MATCH SUB-FORM must be at least two elements long, a matcher
and an expression to evaluate on match. Got %s instead." first-form)
	  (let ((match-expression (car first-form))
			(match-body-exprs (cdr first-form))
			(result-name (gensym "MATCH-HELPER-RESULT-NAME-")))
		`(let ((,result-name 
				(match1 ,match-expression ,value ,@match-body-exprs)))
		   (if (not (eq *match-fail* ,result-name)) ,result-name
			 (match-helper ,value ,@(cdr forms)))))))))


(defmacro* match (value &body forms)
  "Attempt to match VALUE against each of the patterns in the CAR of
FORMS.  When a match is detected, its subsequent forms are executed as
in a PROGN where the bindings implied by the match are in effect.  

An error is thrown when no matches are found."
  (let ((name (gensym "MATCH-VALUE-NAME-")))
	`(let ((,name ,value)) 
	   (match-helper ,name ,@forms))))


(defmacro* match-lambda (&body forms) 
  "Like MATCH except the VALUE is curried."
  (let ((name (gensym "MATCH-LAMBDA-NAME-")))
	`(function (lambda (,name) (match ,name ,@forms)))))

(defun length=1 (lst)
  "Returns T when LST has one element."
  (and (consp lst)
	   (not (cdr lst))))

(defpattern list-rest (&rest patterns)
  (if (length=1 patterns)
	  `(? #'listp ,(car patterns))
	  (let ((pat (car patterns))
			(pats (cdr patterns)))
		`(and (funcall #'car ,pat)
			  (funcall #'cdr 
					   (list-rest ,@pats))))))

(defun cl-struct-prepend (s)
  (intern (format "cl-struct-%s" s)))

(defun make-cl-struct-accessor (struct-name slot) 
  (intern (format "%s-%s" struct-name slot)))


(defpattern struct (struct-name &rest fields)
  `(and
	(? #'vectorp)
	(? #'(lambda (x) (> (length x) 0)))
	(? #'(lambda (o)
		   (eq (elt o 0) ',(cl-struct-prepend struct-name))))
	,@(loop for f in fields collect
			`(funcall 
			  #',(make-cl-struct-accessor struct-name (car f))
			  ,(cadr f)))))




(provide 'shadchen)

