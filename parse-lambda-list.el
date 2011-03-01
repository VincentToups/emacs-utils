(require 'utils)
(require 'multi-methods)
(provide 'parse-lambda-list)

(defvar *default-parse-state*
  (alist>> :state :normal) "Initial state for lambda-list parsing.")

(defun state-changer-p (possible-new-state)
  "Detects whether a token indicates a state change."
  (cond 
   ((eq possible-new-state '&rest) :rest)
   ((eq possible-new-state '&optional) :optional)
   ((eq possible-new-state '&key) :key)
   (t nil)))

(defun state-changer-p-with-checking (possible-new-state old-state)
  "Detects a state change but also makes sure that the change is valid for a lambda list.  Any state 
may follow after normal arguments, but only keywords can follow after optional arguments."
  (let-if new-state (state-changer-p possible-new-state)
		  (let ((pair (list old-state new-state)))
			(cond
			 ((equal '(:normal :rest) pair) new-state)
			 ((equal '(:optional :rest) pair) new-state)
			 ((equal '(:normal :key)  pair) new-state)
			 ((equal '(:normal :optional) pair) new-state)
			 ((equal '(:optional :key) pair) new-state) 
			 (t (error "Malformed lambda-list"))))
		  nil))

(defun lambda-list-reducer (item acc)
  "The reducing function for parsing a lambda list by folding.  Each step detects a state change or adds a
token to the appropriate key in the accumulation alist."
  (let-alist ((state :state)) acc
			 (let-if new-state (state-changer-p-with-checking item state)
					 (alist>> acc :state new-state
							  state (reverse (alist acc state)))
					 (alist-cons acc
								 state item))))

(defun* parse-lambda-list (lambda-list &key (extended t))
  "Parses a (common-lisp-like) lambda list using a fold and a LAMBDA-LIST-REDUCER.  Checks for malformed
argument lists.  Returns an association-list for the :normal, :key, :optional and :rest parts of the list."
  (reverse (alist-conjugate (dissoc (foldl #'lambda-list-reducer *default-parse-state* lambda-list) :state)
							:rest
							#'car nil)))

(defun lambda-list-sub-form-get-name (sub-form)
  "Returns the symbol part of a lambda-list sub form: (x 10) or (x) -> x, but y -> y."
  (if (listp sub-form) (car sub-form)
	sub-form))

(defun lambda-list-names-in-order (lambda-list)
  "Return a list of the symbols in lambda-list, in the order they were encountered."
  (loop for item in (parse-lambda-list lambda-list) append
		(if item
			(case (car item)
			  ((:rest) (if (cadr item) (list (cadr item)) nil))
			  (otherwise (mapcar #'lambda-list-sub-form-get-name (cadr item))))
		  nil)))

(defun lambda-list-names-in-order-explicit-rest-list (lambda-list)
  "Return a list of the symbols in lambda-list, in the order they were encountered.
In this version of the function, the rest form is enclosed in a list, for subsequent macro magic."
  (loop for item in (parse-lambda-list lambda-list) append
		(if item
			(case (car item)
			  ((:rest) (if (cadr item) (list `(list ,(cadr item))) nil))
			  (otherwise (mapcar #'lambda-list-sub-form-get-name (cadr item))))
		  nil)))


(dont-do

 ;example
 (defun* tttt (a b c &optional (x 10) (y 11) &key (z 13)) (list a b c x y z))
 (tttt 1 2 3 4 'y :z 11)

   (cl-prettyprint (parse-lambda-list '(a b c &optional (x 10) (y 11) &key (z 13))))
((:rest nil)
 (:key ((z 13)))
 (:optional ((x 10) (y 11)))
 (:normal (a b c)))



)



