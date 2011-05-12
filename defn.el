(require 'cl)
(require 'utils)
(require 'parse-table-binder)
(require 'parse-seq-binder)

(defvar currently-defining-defn 'lambda 
  "The function we are currently defining.  Useful for
  informative error messages.")

(defun binder->type (f)
  "Inspect a binding form to determine its binder type.
Empty vectors, and vectors whose first element is not :: indicate a sequence.
Vectors whose first element is :: indicate a table (either a hash or an alist).
Other forms produce an error."
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

(defun wrap-or-form (form)
  "Or expressions are evaluated at call-time, 
and are hence delayed with a lambda expression."
  `(lambda () ,form))

(defun handle-seq-binding (binder expr previous-lets)
  "This function takes a binding expression (BINDER) and a series
of value-producing expressions (EXPR) and produces a list of
name-expression pairs which represents that destructuring.  These
will be further expanded if necessary, but finally inserted into
a `let*` form.  The body of that let form will be a scope where
the indicated bindings are made.

PREVIOUS-LETS allows this function to be called in situations
where binding forms need to be accumulated."
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

										; (handle-seq-binding [a b c d :or [1 2 3 4]] '(list 1 2 3 4) '())
										; (handle-seq-binding [] '() '())

(dont-do
 ;;; These `dont-d` expressions are not evaluated when the file is loaded,
 ;;; but usually contain the equivalent of unit tests.
 (handle-seq-binding [x] '(1) '())
 
 (handle-seq-binding [x [a b]] '(list 1 (list 1 2)) '())
 ;; ([lambda-seq-as-sym99989 (list 1 (list 1 2))] 
 ;;  [x (elt lambda-seq-as-sym99989 0)] 
 ;;  [lambda-seq-as-sym99995 (elt lambda-seq-as-sym99989 1)] 
 ;;  [a (elt lambda-seq-as-sym99995 0)] 
 ;;  [b (elt lambda-seq-as-sym99995 1)])

;;; Example above: [x [a b]] indicates we wish to destructure a list
;;; containing at least two elements, the second of which should be
;;; further destructured into the variables `a` and `b`. 

;;; The output should be read as binding expressions in a let: first
;;; we bind the whole input expression to a hidden variable then we
;;; bind x to the first element of that value then we bind the inner
;;; list to another hidden variable then bind a and b to the car and
;;; cadr of that value.  We use the generic function from cl.el, elt,
;;; so that sequences (lists, strings, vectors) can be destructured by
;;; the same syntax.
 )


(defun handle-tbl-binding (binder expr previous-lets)
  "Handle destructuring a table expression (BINDER) on the
expression EXPR.  PREVIOUS-LETS allow this function to take an
accumulation variable."
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

(dont-do
 (handle-tbl-binding [:: [a b] :x y :y] '(tbl 'x 10 'y 11) '())
 ;; (
 ;;  [lambda-as-symbol38977 (tbl (quote x) 10 (quote y) 11)] 
 ;;  [lambda-seq-as-sym38983 (table-like-get lambda-as-symbol38977 :x)] 
 ;;  [a (elt lambda-seq-as-sym38983 0)] 
 ;;  [b (elt lambda-seq-as-sym38983 1)] 
 ;;  [y (table-like-get lambda-as-symbol38977 :y)])

;;; Table binding is somewhat more complex.  [:: [a b] :x ] means:
;;; "destructure a table by binding a and b to the sequence stored in
;;; the :x slot of of the table, and then bind y to the value stored
;;; in the :y slot.
 )

(defun* handle-binding (binder expr &optional
							   (previous-lets '()))
  "Handle binding orchestrates symbol, table and sequence binding
operations.  BINDER is the binding expression, and previous-lets
allows this function to take an accumulation of let bindings.
This is not used here, but is used when called recursively."
  (case (binder->type binder) 
	(:symbol (append previous-lets (list (vector binder expr))))
										;symbol binding is trivial.
	(:seq (handle-seq-binding binder expr previous-lets))
	(:tbl (handle-tbl-binding binder expr previous-lets))))

										; (handle-binding [a [:: [a b :as lst] :s :as table] & rest] 10)

(defun package-dlet-binders (pairs)
  "Converts a set of [bindexpr expr] pairs into a pair
  [bind-expressions expressions] so that it can be used by handle-binding."
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
  "Convert the output of `handle-binding` to forms appropriate
for a regular `let*` expression."
  (mapcar (lambda (x) (coerce x 'list))
		  (apply #'handle-binding (package-dlet-binders pairs))))

(defun split-after-two (lst)
  "Split LST after two elements."
  (list (elts lst (range 2))
		(elts lst (range 2 (length lst)))))

(defmacro* dlet (pairs &body body)
  "Clojure-style destructuring let expression.
 (DLET [bind-form bind-val ...] body) destructures the values
 BIND-VAL via BIND-FORMS and executes BODY in a context where the
 symbols in BIND-FORMs are defined.  

This macro provides a lexical-scope for the bound variables.  Use
dlet_ for a dynamic scope."
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
  "Identical to DLET except variables in BIND-FORMS are bound dynamically."
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

(defun build-dsetq (pairs)
  "Build a dsetq expansion form."
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
  "PAIRS is a series of BIND-EXPR VALUE doublets, in a flat list.
Set the symbols indicated in BIND-EXPRs to the values indicated
in VALUEs with Clojure-style destructuring."
  (if (oddp (length pairs)) (error "dsetq needs an even number of elements.")
	(build-dsetq pairs)))
										; (dlet [[a b] (list 10 10) y 11] (+ a b y))
										; (dlet [[x y :as z] (list 1 2) b (+ x y)] (list x y b z))

(defun generate-defn-exprs (arg-nam n)
  "Generate forms extracting the NTH element of ARG-NAME.
Because top-level destructuring of `defn` forms is always
sequential, we can get away with this."
  (loop for i from 0 below n collect
		`(elt ,arg-nam ,i)))

(defun binder-arity (binder)
  "Given a BINDER expression, calculate the arity of the function
as either (N exactly) or (N +MORE).  Used to dispatch FN and DEFN
expressions to the appropriate body based on function arity."
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
  "Given N and an ARITY, determine if N matches that ARITY.  If
ARITY is exact and numericall equal to N, match.  If ARITY is
+MORE and n is greater than or equal to ARITY's value, match.
-LESS is unused as yet (Clojure does not suppor this)."
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
  "Compare two arities.  Handles EXACTLY, +MORE and -LESS correctly."
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
  "Sort arities."
  (sort* lst #'arity-comparitor))

(defun random-arity ()
  "Produce a random ARITY value (for testing)."
  (let ((mods '(exactly +more -less)))
	(list (random 20) (elt mods (random 3)))))

										; (random-arity)

										; (sort-arities (foldl (lambda (it ac) (cons (random-arity) ac)) '() '(1 2 3 4 5 6)))


										; (arity-match 2 '(3 +more))
										; (arity-match 3 '(3 -less))
										; (arity-match 2 '(2 exactly))
										; (arity-match 2 '(3 -less))

										; (handle-seq-binding [a b c & rest :or (list 10 11 12 13 14 15 16)] '(1 2 3 4 5) '())

										; (arity-match 2 '(3 +more))

(defun gen-fn-rec-binding (binders args-sym)
  "Generates binding forms for DSETQ expressions in a recur."
  (vector (coerce binders 'vector) args-sym))

(defmacro* fnc (&rest rest)
  "Version of FN which compiles itself before returning.  Useful
sometimes."
  `(byte-compile (fn ,@rest)))
(defmacro* fnc_ (&rest rest)
  "Version of FN_ which compiles itself before returning.  _
indicates that this macro creates dynamic variable bindings."
  `(byte-compile (fn_ ,@rest)))

(defmacro* fn (&rest rest)
  "Clojure-style destructuring lambda.
 Example: (funcall (fn [[x y & r]] (list x y r)) '(1 2 3 4 5 6))
 -> (1 2 (3 4 5 6)).  Supports dispatch to bodies based on input
 arity and full, recursive destructuring.  Lists are destructured
 using [a b c] style expressions, while tables are destructured
 using [:: symbol key ... ] expressions.

Both lists and tables support the :or keyword, which specifies a
table or a list respectively to destructure if a desired element
is not in the passed in structure.

The keyword :as indicates that a structure itself should be given
a name.

The expression (RECUR ...) inside the body, in tail position
only, will simulate a tail self-recursion.  If the RECUR is not
in tail position, an error is generated at parse time.  Recur
otherwise has function call semantics (although APPLY #'recur is
not supported).

See DEFN for more extensive examples.

"
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
  "See FN.  This macro is identical except it binds its variables with a dynamic scope."
  (cond
   ((vectorp (car rest)) ;;; detect single arity FN
	`(fn_ (,(car rest) ,@(cdr rest)))) ;;; recursive macro expansion.
   ((listp (car rest))	   ; set of different arity binders/bodies
	(let ((args-sym (gensym))
		  (numargs (gensym)))
	  `(lambda (&rest ,args-sym) 
		 (let ((,numargs (length ,args-sym))) ;;; count number of args for arity match.
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
										  ,@expanded-body) ;;; body with recur support
									  `(let* ,(mapcar 
											   (lambda (x) (coerce x 'list)) 
											   (handle-binding binders args-sym)) ,@body)))) ;;; body without recur support.
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
  "Determines if a DEFN form declares itself INTERACTIVE and
strips that declaration out, returning it and the new DEFN
expression.  Needed to support INTERACTIVE defns."
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
  "DEFUNC defines, then compiles, a function.  Syntactically
identical to a regular DEFUN."
  (let ((retsym (gensym "defunc-val")))
	`(let ((,retsym (defun ,@rest)))
	   (byte-compile ',(car rest))
	   ,retsym)))

(defmacro* defn (name &rest rest)
  "Clojure-style function definition.  Supports recur and destructuring bind.
The argument list is a VECTOR of Symbols,
Table-binding-expressions ([:: ...]) or sequence binding
expressions [...]. 

For example:

 (defn example [a b c] (list a b c)) 

takes three arguments and returns a list of each.

 (defn example [[a b] c] (list a b c))

Takes two arguments, the first of which is a list, whose first
and second values are bound to a and b respectively.  The second
is bound to c, and these values are returned.

 (defn example [[:: a :x b :y :as c]] (list a b c))

Takes ONE argument, a table, and binds a to the value at :x, b to
the value at :y, and c to the able itself, returning a list of
the three.  The :as keyword works on lists also.

 (defn example [& rest] rest) 

Takes an unlimited number of arguments and returns them as a list.

DEFN can dispatch on arity at call time.  For instance

 (defn prod ([[head & tail :as lst] acc]
       (if lst (recur tail (* acc head))
           acc))
       ([lst]
        (prod lst 1)))

 (prod '(1 2 3 4 5)) ;-> 120 

Takes either two arguments (first clause) or one
argument (second).  The second calls PROD with an additional
argument.  Within the body of PROD, RECUR is used to repeatedly
call PROD without growing the stack, finally returning the result
of multiplying the items of LST.  This example also uses nested
destructuring and the :as keyword.

"
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
  "See DEFN.  This is identical except variables specified in NAME are bound dynamically."
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

;;; We need a set of functions for controlling the codewalker which expands RECUR.

(defun ifp (form) 
  "True of FORM is an IF expression."
  (and (listp form)
	   (eq (car form) 'if)))
(defun condp (form)
  "True if a form is a COND expression."
  (and (listp form)
	   (eq (car form) 'cond)))
(defun casep (form)
  "True if a form is a CASE expression."
  (and (listp form)
	   (eq (car form) 'case)))
(defun recurp (form)
  "True if the head of a form is RECUR."
  (and (listp form)
	   (eq (car form) 'recur)))
(defun prognp (form)
  "True if a form is a PROGN form."
  (and (listp form)
	   (eq (car form) 'progn)))
(defun expand-recur-cond-pair (cond-pair parent-is-tail loop-sentinal binding-forms)
  "Expand a pair in a COND expression."
  `(,(car cond-pair)
	,@(cdr (expand-recur `(progn ,@(cdr cond-pair)) parent-is-tail loop-sentinal binding-forms))))
;; (defun expand-recur-recur (form parent-is-tail loop-sentinal binding-forms)
;;   `(progn 
;; 	 (setq ,loop-sentinal t)
;; 	 (dsetq ,@(loop for b in (coerce binding-forms 'list) and v in (cdr form) 
;; 					collect b and collect v))))
(defun expand-recur-recur (form parent-is-tail loop-sentinal binding-forms)
  "Actuall expand a RECUR expression into a set expression.  PARENT-IS-TAIL must be true.
LOOP-SENTINAL is the symbol which determines if the recursion
continues.  BINDING-FORMS are expeanded into a set statement."
  (if parent-is-tail
	  `(progn 
		 (setq ,loop-sentinal t)
		 (dsetq ,@binding-forms (list ,@(cdr form))))
	(error "Recur expression \"%S\" not in tail position in %s." form currently-defining-defn)))

(defun let-likep (form)
  "Detect let-like forms (let, flet, labels, lexical-let,
lexical-let*.)"
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

(defun* expand-recur (form parent-is-tail loop-sentinal binding-forms &optional (single-arg-recur nil))
  "Recursively search for and expand RECUR forms in FORM, appropriately setting the LOOP-SENTINAL."
  (let ((mxform (macroexpand form)))
	(cond ((symbolp mxform) mxform)
		  ((numberp mxform) mxform)
		  ((stringp mxform) mxform)
		  ((arrayp  mxform) mxform)
		  ((listp mxform)
		   (case parent-is-tail
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
						parent-is-tail 
						loop-sentinal 
						binding-forms))
					 (cdr mxform))))
			   ((casep mxform)
				`(case ,(cadr mxform)
				   ,@(mapcar 
					  (lambda (cond-pair) 
						(expand-recur-cond-pair 
						 cond-pair 
						 parent-is-tail 
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
											parent-is-tail loop-sentinal binding-forms)
				(expand-recur-recur mxform parent-is-tail loop-sentinal binding-forms)))
			   (t (progn
					(if (> (length (filter (lambda (x) (and (symbolp x) (eq 'recur x))) (flatten mxform))) 0)
						(error (format "Can't recur from a non-tail position in %s" mxform)))
					mxform)))))))))

(defmacro* dloop-single-arg (bindings &body body)
  "Suppport form for recursive looping.  Similar to dlet or dloop
but takes only a single binding expression."
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
  "See dloop-single-arg_.  This version creates a dynamic scope instead."
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
  "Equivalent of a clojure LOOP form.  Syntactically identical to
a LET expression, but RECUR calls the loop form as if it were a function.
Bindings support full destructuring."
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
									 ,(expand-recur `(progn ,@body) t loop-sentinal (list (list->vector binding-parts)) ))))
		 ,return-value))))

(defmacro* dloop_ (bindings &body body)
  "See DLOOP.  Dynamic version."
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
									 ,(expand-recur `(progn ,@body) t loop-sentinal (list (list->vector binding-parts)) ))))
		 ,return-value))))



(provide 'defn)
; provide defn and friends to the world.

