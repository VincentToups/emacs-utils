(require 'defn)
(require 'utils)
(require 'functional)
(require 'cl)
(provide 'multi-methods)

(defvar *hierarchy-weak-table* (make-hash-table :test 'eql :weakness t) "Weak table for keeping track of hierarchies.")

(defvar *multi-method-heirarchy* (alist>> :down nil
										  :up nil
										  :resolutions nil) "The default multimethod hierarchy used for isa? dispatch.")

(defun make-hierarchy ()
  "Create a hierarchy for multi-method dispatch."
  (let ((tag (gensym "hierarchy-tag")))
	(puthash tag t  *hierarchy-weak-table*)
	(alist>> ::::hierarchy-tag tag)))

(defun hierarchyp (object)
  "Tests to see if an object is a hierarchy."
  (and (listp object)
	   (let-if tag (alist object ::::hierarchy-tag)
			   (gethash tag *hierarchy-weak-table* )
			   nil)))

(defun mk-dispatch-table-name (method)
  "generates the symbol for a dispatch table for METHOD"
  (internf "--%s-dispatch-table" method))

(defun mk-dispatch-hierarchy-name (method)
  "generates the symbol for hierarchy name for METHOD"
  (internf "--%s-hierarchy-table" method))

(defun mk-dispatch-function-name (method)
  "generates the symbol for the dispatch function for METHOD"
  (internf "--%s-dispatcher" method))

(defun mk-default-method-name (method)
  "generates the symbol for the default method for METHOD"
  (internf "--%s-default-method" method))

(defun make-keyword-accessor (kw)
  "Creates an accessor for tables looking for KW"
  (lexical-let ((kw kw))
	(lambda (table &rest args) (table-like-get table kw))))

(defun clear-dispatch-cache-raw ()
  "Clear the dispatch cache for the hierarchy in the dynamic scope."
  (alist! *multi-method-heirarchy* ::::dispatch-cache nil)
  t)

(defun clear-dispatch-cache (&rest args)
  "Retrieve the cache of dispatches for the currently scoped hierarchy, or for one passed in."
  (case (length args)
	((0) (clear-dispatch-cache-raw))
	((1) (let ((*multi-method-heirarchy* (car args)))
		   (clear-dispatch-cache-raw)))
	(otherwise 
	 (error "clear-dispatch-cache: Takes either 0 or 1 arguments."))))

(defun over-all-args (kw/f)
  (lexical-let ((kw/f kw/f))
	(if (functionp kw/f)
		(lambda (&rest args)
		  (map 'vector kw/f args))
	  (lambda (&rest args)
		(map 'vector 
			 (lambda (tble) (table-like-get tble kw/f))
			 args)))))

(defun macro-functionp (object)
  (cond 
   ((functionp object) t)
   ((and (listp object)
		 (= 2 (length object))
		 (eq (car object) 'function)))
										;(functionp (cadr object)))) ;
   (t nil)))


(defmacro* defmulti (name dispatch &optional (doc "") (hierarchy-expression '*multi-method-heirarchy*))
  "Define a multi-method NAME with dispatch function DISPATCH.  DEFUNMULTI defines specific instances of the method."
  (let ((table-name (mk-dispatch-table-name name))
		(default-method-name (mk-default-method-name name))
		(dispatch-name (mk-dispatch-function-name name))
		(args-name (gensymf "multi-%s-args" name))
		(internal-name (gensymf "multi-%s-holder" name))
		(hierarchy-name (mk-dispatch-hierarchy-name name))
		(temp (gensym)))
	`(progn 
	   (defvar ,default-method-name nil)
	   (defvar ,hierarchy-name nil ,(format "dispatch hierarchy for %s" name))
	   (setq ,hierarchy-name ,hierarchy-expression)
	   (defvar ,table-name (alist>>) ,(format "dispatch-table for %s" name))
	   (setq ,table-name (alist>>))
	   (let ((,temp ,dispatch))
		 (defvar ,dispatch-name ,temp ,(format "dispatch-function for %s" name))
		 (setq ,dispatch-name ,temp)
		 (unless (functionp ,dispatch-name) 
		   (print (format "Creating a dispatch function for %S.  You may need to define %S before declaring the multimethod if you don't mean to use table-based dispatch." ,dispatch-name ,dispatch-name))
		   (setq ,dispatch-name (make-keyword-accessor ,dispatch-name))))
	   (defun ,name (&rest ,args-name)
		 ,doc
		 (let* ((*multi-method-heirarchy* ,hierarchy-name)
				(,internal-name (isa-dispatch-memo (apply ,dispatch-name ,args-name) ,table-name (make-resolve-by-table (alist *preferred-dispatch-table* ',name) ',name ) ,default-method-name)))
		   (if ,internal-name (apply ,internal-name ,args-name)
			 (error (format ,(format "No known method for args %%S for multimethod %s.\n  Dispatch value is: %%S" name) ,args-name (apply ,dispatch-name ,args-name)))))))))

(defmacro* defunmethod-default (name arglist &body body)
  "Define a method of last resort for the method NAME, called when no match is available in the dispatch table."
  `(progn 
	 (let ((*multi-method-heirarchy* ,(mk-dispatch-hierarchy-name name)))
	   (clear-dispatch-cache))
	 (setq ,(mk-default-method-name name)
		   (lambda ,arglist 
			 ,@body))
	 ',name))

(defmacro* undefunmethod (name value)
  "Undefine the method for the multimethod NAME and dispatch value VALUE."
  (let ((table-name (mk-dispatch-table-name name)))
	`(let ((*multi-method-heirarchy* ,(mk-dispatch-hierarchy-name name)))
	   (clear-dispatch-cache)
	   (setq ,table-name 
			 (dissoc-equal ,table-name ,value)))))


(defmacro* defunmethod (name value arglist &body body)
  "Define a method using DEFUN syntax for the dispatch value VALUE."
  (let ((g (gensym))
		(table-name (mk-dispatch-table-name name)))
	`(let ((,g (lambda ,arglist ,@body)))
	   (let ((*multi-method-heirarchy* ,(mk-dispatch-hierarchy-name name)))
		 (clear-dispatch-cache))
	   (setq ,table-name 
			 (alist-equal>> ,table-name ,value ,g))
	   ',name)))

(defvar *preferred-dispatch-table* nil "Table of method dispatch resolution rules.")
(defun prefer-method-fun (name pref-val not-pref-val)
  "Indicate that the NAMEd multimethod should prefer PREF-VAL over NOT-PREF-VAL when dispatching ambiguous inputs."
  (let ((subtbl (alist *preferred-dispatch-table* name)))
	(alist! subtbl (vector pref-val not-pref-val) pref-val)
	(alist! subtbl (vector not-pref-val pref-val) pref-val)
	(setf (alist *preferred-dispatch-table* name) subtbl)))

(defmacro prefer-method (name pref-val not-pref-val)
  "Declare that a particular dispatch value PREF-VAL is preferred over NOT-PREF-VAL when dispatching the NAMEd method."
  `(prefer-method-fun ',name ,pref-val ,not-pref-val))






(defun clear-mm-heirarchy ()
  "Clear the hierarchy in the dynamic scope. "
  (setq *multi-method-heirarchy* (alist>> :down nil
										  :up nil
										  :resolutions nil))
  *multi-method-heirarchy*)

(dont-do 
 (setq *multi-method-heirarchy* (alist>> :down nil
										 :up nil))
 (add-parent-relation :vector :thing)
 (add-child-relation :thing :vector))

(defun add-parent-relation (child parent)
  "Add a PARENT CHILD relationship to the hierarchy in the dynamic scope."
  (let ((parents (alist *multi-method-heirarchy* :up)))
	(setf (alist *multi-method-heirarchy* :up) (alist-add-to-set parents child parent)))
  *multi-method-heirarchy*)

(defun remove-parent-relation (child parent)
  "Add a PARENT CHILD relationship to the hierarchy in the dynamic scope."
  (let ((parents (alist *multi-method-heirarchy* :up)))
	(setf (alist *multi-method-heirarchy* :up) (alist-remove-from-set parents child parent)))
  *multi-method-heirarchy*)

(defun add-child-relation (parent child)
  "Add a CHILD PARENT relationship to the hierarchy in the dynamic scope."
  (let ((children (alist *multi-method-heirarchy* :down)))
	(setf (alist *multi-method-heirarchy* :down) (alist-add-to-set children parent child)))
  *multi-method-heirarchy*)

(defun remove-child-relation (parent child)
  "Removes a CHILD PARENT relationship to the hierarchy in the dynamic scope."
  (let ((children (alist *multi-method-heirarchy* :down)))
	(setf (alist *multi-method-heirarchy* :down) (alist-remove-from-set children parent child)))
  *multi-method-heirarchy*)

(defun derive2 (parent child)
  "Declare a PARENT-CHILD relationship in the dynamically scoped hierarchy."
  (clear-dispatch-cache)
  (add-child-relation parent child)
  (add-parent-relation child parent))

(defun underive2 (parent child)
  (clear-dispatch-cache)
  (remove-child-relation parent child)
  (remove-parent-relation child parent))

(defun derive (&rest args)
  "derive H PARENT CHILD establishes a parent-child relationship in H, a heirarchy.
   derive PARENT CHILD uses the default hierarchy."
  (case (length args)
	((2) (apply #'derive2 args))
	((3) (let ((*multi-method-heirarchy* (car args)))
		   (apply #'derive2 (cdr args))))
	(t "Derive takes 2 or 3 arguments.  More or less were given.")))

(defun underive (&rest args)
  "derive H PARENT CHILD establishes a parent-child relationship in H, a heirarchy.
   derive PARENT CHILD uses the default hierarchy."
  (case (length args)
	((2) (apply #'derive2 args))
	((3) (let ((*multi-method-heirarchy* (car args)))
		   (apply #'derive2 (cdr args))))
	(t "Derive takes 2 or 3 arguments.  More or less were given.")))


(defun* derives-from (child parent &optional (h *multi-method-heirarchy*))
  (let ((*multi-method-heirarchy* h))
	(derive2 parent child)))

(defun* derive-from (children parent &optional (h *multi-method-heirarchy*))
  (let ((*multi-method-heirarchy* h))
	(loop for child across (coerce children 'vector) do
		  (derives-from child parent h))))

(defun mm-parents (child)
  "Get the PARENTS of CHILD from the hierachy in the dynamic scope."
  (let ((parents (alist *multi-method-heirarchy* :up)))
	(alist parents child)))

(defun mm-children (parent)
  "Get the CHILDREN of PARENT from the hierachy in the dynamic scope."
  (let ((children (alist *multi-method-heirarchy* :down)))
	(alist children parent)))

(defun mm-ancestors (child)
  "Get all the ancestors of CHILD."
  (let* ((parents (mm-parents child))
		 (ancestors parents)
		 (done
		  (if parents nil t)))
	(loop while (not done) do
		  (let ((above (unique (map&filter #'identity #'mm-parents parents) #'equal)))
			(if above 
				(progn 
				  (setq parents above)
				  (setq ancestors (apply #'append (cons ancestors above))))
			  (setq done t))))
	ancestors))

(defun mm-descendants (child)
  "Get all the descendants of CHILD."
  (let* ((children (mm-children child))
		 (descendants children)
		 (done
		  (if children nil t)))
	(loop while (not done) do
		  (let ((below (unique (map&filter #'identity #'mm-children children) #'equal)))
			(if below 
				(progn 
				  (setq children below)
				  (setq descendants (apply #'append (cons descendants below))))
			  (setq done t))))
	descendants))

(defun get-method (name dispatch-value)
  "Get the multimethod of kind NAME that is the nearest match for the DISPATCH-VALUE."
  (let* ((method-table-name (mk-dispatch-table-name name))
		 (method-table (eval method-table-name)))
	(isa-dispatch dispatch-value method-table (make-resolve-by-table method-table name))))

(defun get-method-funcall (name dispatch-value &rest args)
  "Get the method associated with NAME and DISPATCH-VALUE and call it on ARGS."
  (let ((m (get-method name dispatch-value)))
	(if m (apply m args)
	  (error "get-method-funcall: No method for %s with dispatch value %S." name dispatch-value))))

(defun get-method-apply (name dispatch-value args)
  "Get the method associated with NAME and DISPATCH-VALUE and call it on ARGS, a list."
  (let ((m (get-method name dispatch-value)))
	(if m (apply m args)
	  (error "get-method-funcall: No method for %s with dispatch value %S." name dispatch-value))))

										; declare some testing hierarchy
(derive :thing :parseable)
(derive :thing :number)
(derive :thing :collection)
(derive :collection :list)
(derive :collection :vector)
(derive :parseable :string)
(derive :parseable :buffer)

(defun isa_ (o1 o2)
  "Underlying implementation of isa on regular objects."
  (if (equal o1 o2) 0
	(let* ((parents (mm-parents o1))
		   (done (if parents nil t))
		   (rank (if parents 1 nil)))
	  (loop while (not done) do
			(if (any (mapcar (cr #'equal o2) parents))
				(setq done t)
			  (progn 
				(setq rank (+ rank 1))
				(setq parents 
					  (apply #'append (mapcar #'mm-parents parents)))
				(unless parents 
				  (setq done t)
				  (setq rank nil)))))
	  rank)))

(defmacro lazy-and2 (e1 e2)
  "A lazy and macro."
  (let ((e1- (gensym "lazy-and-e1-")))
	`(let ((,e1- ,e1))
	   (if (not ,e1-) nil (and ,e1- ,e2)))))

(defun count-equilength-vectors (list-of)
  "Return the number of objects in list-of which are equilength vectors."
  (reduce #'+ 
		  (let ((n nil))
			(mapcar 
			 (lambda (v?)
			   (if (vectorp v?)
				   (progn 
					 (if (not n) 
						 (progn 
						   (setq n (length v?))
						   1)
					   (if (= n (length v?)) 1 0)))
				 0))
			 list-of))))

(defun isa? (o1 o2)
  "ISA? test for equality using the default hierarchy.  Child ISA? Parent but not vice versa.  Isa? returns a number representing the distance to the nearest ancestor that matches.  For vectors of objects, these distances are summed.  If nil, o1 is not an o2."
  (case (count-equilength-vectors (list o1 o2))
	((0) (isa_ o1 o2))
	((1) nil)
	((2) (reduce (lambda (a b)
				   (cond 
					((and (numberp a)
						  (numberp b))
					 (+ a b))
					(t nil)))
				 (map 'vector #'isa_ o1 o2)))))

(defun resolve-by-first (o r p1 p2)
  "Default, dumb conflict resolver."
  (list r p1))

(defun make-resolve-by-table (resolution-table method-name)
  "Creates a conflict resolution function which checks to see if a method has a specific conflict resolution procedure defined."
  (lexical-let ((restbl resolution-table)
				(method-name method-name))
	(lambda (object rank p1 p2)
	  (let-if resolution (alist restbl (vector (car p1) (car p2)))
			  (list rank (alist (list p1 p2) resolution))
			  (error "Method dispatch ambiguity for %s unresolved (%S vs %S)." method-name (car p1) (car p2))))))

(defun get-dispatch-cache-raw ()
  "Get the dispatch cache for the hierarchy in the dynamic scope.  Create one if not available."
  (let-if cache (alist *multi-method-heirarchy* ::::dispatch-cache)
		  cache
		  (let ((cache (make-hash-table :test 'equal))) 
			(alist! *multi-method-heirarchy* ::::dispatch-cache cache)
			cache)))

;; (defun clear-dispatch-cache-raw ()
;;   "Clear the dispatch cache for the hierarchy in the dynamic scope."
;;   (alist! *multi-method-heirarchy* ::::dispatch-cache nil)
;;   t)

;; (defun clear-dispatch-cache (&rest args)
;;   "Retrieve the cache of dispatches for the currently scoped hierarchy, or for one passed in."
;;   (case (length args)
;; 	((0) (clear-dispatch-cache-raw))
;; 	((1) (let ((*multi-method-heirarchy* (car args)))
;; 		   (clear-dispatch-cache-raw)))
;; 	(otherwise 
;; 	 (error "clear-dispatch-cache: Takes either 0 or 1 arguments."))))

(defun get-dispatch-cache (&rest args)
  "Retrieve the cache of dispatches for the currently scoped hierarchy, or for one passed in."
  (case (length args)
	((0) (get-dispatch-cache-raw))
	((1) (let ((*multi-method-heirarchy* (car args)))
		   (get-dispatch-cache-raw)))
	(otherwise 
	 (error "get-dispatch-cache: Takes either 0 or 1 arguments."))))

(defun* isa-dispatch-memo (object alist resolver &optional (default-method nil))
  "Dispatch from an alist table based on ISA? matches.  More specific matches are preferred over less, and ambiguous matches will be resolved by the function resolver. (memoized)"
  (let* ((cache (get-dispatch-cache))
		 (method (gethash (list object resolver default-method) cache)))
	(if method method
	  (let ((method (isa-dispatch object alist resolver default-method)))
		(puthash (list object resolver default-method) method cache)
		method))))

(defun* isa-dispatch (object alist resolver &optional (default-method nil))
  "Dispatch from an alist table based on ISA? matches.  More specific matches are preferred over less, and ambiguous matches will be resolved by the function resolver."
  (let-if method (cadr (cadr (foldl 
							  (lambda (alist-pair best-so-far)
								(let ((rank (isa? object (car alist-pair))))
								  (cond
								   ((not rank)  best-so-far)
								   ((not best-so-far) (list rank alist-pair))
								   ((< rank (car best-so-far))
									(list rank alist-pair))
								   ((> rank (car best-so-far)) best-so-far)
								   ((= rank (car best-so-far))
									(if rank
										(funcall resolver object rank alist-pair (cadr best-so-far)) nil)))))
							  nil
							  alist)))
		  method
		  default-method))

(dont-do
										;example
 (defmulti report :student-name)
 (defunmethod report :ricky-gervais (student) "I got an A+")
 (defunmethod report :karl-pilkington (student) "Maybe I forgot to sign up for exams.")
 (report (alist>> :student-name :ricky-gervais)) ;-> "I got an A+"
 (report (alist>> :student-name :karl-pilkington)) ;-> "Maybe I forgot to sign up for exams.")
 (report (alist>> :steven-merchant)) ;-> error, no method
 )
