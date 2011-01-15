(require 'macro-utils)

(defun nil->0 (x)
  (if x x 0))

(defun symbol-binding-info (sym local-info)
  (dlet [[:: [sb fb :or (list nil nil)] sym :or (alist>> sym (list nil nil))] local-info]
	(list sb fb)))

(defun symbol-bind-counts (sym global-info)
  (dlet [[:: [sc fc suc fuc :or (list 0 0 0 0)] sym or (alist>> sym (list 0 0 0 0))] global-info]
	(list sc fc suc fuc)))

(defun as-progn (lst)
  `(progn ,@lst))

(defun collect-usage-info-let (form global-info local-info)
  (let* ((body (get-let-body form))
		 (bindings (get-let-binders form))
		 (bound (mapcar #'car bindings))
		 (expressions (mapcar #'cadr bindings))
		 (new-local (apply #'alist>>
						   (loop for b in bound collect b collect
								 (let ((bindinf (alist local-info b)))
								   (if bindinf (list t (cadr bindinf))
									 (list t nil))))))
		 (new-global (foldl (la (expr global)
								(collect-usage-info expr global local-info))
							global-info
							expressions)))
	(collect-usage-info-prog-like body new-global new-local)))

(defun collect-usage-info-let* (form global-info local-info)
  (let* ((body (get-let-body form))
		 (bindings (get-let-binders form)))
	(loop with 
		  accumg = global-info and
		  accuml = local-info and
		  last-bound = nil
		  for binding in bindings do
		  (let-seq (symbol-name value-expression) binding 
				   (setf accumg 
						 (collect-usage-info value-expression accumg accuml))
				   (setf accuml
						 (let-seq (bound-s bound-f) (symbol-binding-info symbol-name accuml)
								  (alist>> accuml symbol-name (list t bound-f)))))
		  finally (return (collect-usage-info-prog-like body accumg accuml)))))

(defun collect-usage-info-prog-like (form global-info local-info)
  (if (or (eq (car form) 'progn) 
		  (eq (car form) 'prog1))
	  (collect-usage-info-prog-like (cdr form) global-info local-info)
	(foldl (lexical-let ((li local-info))
			 (la (sub-form glob-inf)
				 (collect-usage-info sub-form glob-inf li)))
		   global-info
		   form)))

;; (defun collect-usage-info-function (form global-info local-info)
;;   (let* ((symbol-name (cadr form))
;; 		 (binding-info (symbol-binding-info symbol-name local-info))
;; 		 (f-bound (cadr binding-info)))
;; 	(let-seq (s-count f-count su-count fu-count) (symbol-bind-counts symbol-name global-info)
;; 			 (alist>> global-info symbol-name 
;; 					  (list s-count 
;; 							(if f-bound (+ 1 f-count) f-count)
;; 							su-count
;; 							(if (not f-bound) 
;; 								(+ 1 fu-count)
;; 							  fu-count))))))

(defun collect-usage-info-function (form global-info local-info)
  (let* ((symbol-name (cadr form)))
	(if (symbolp symbol-name)
		(progn 
		  (let* ((binding-info (symbol-binding-info symbol-name local-info))
				 (f-bound (cadr binding-info)))
			(let-seq (s-count f-count su-count fu-count) (symbol-bind-counts symbol-name global-info)
					 (alist>> global-info symbol-name 
							  (list s-count 
									(if f-bound (+ 1 f-count) f-count)
									su-count
									(if (not f-bound) 
										(+ 1 fu-count)
									  fu-count))))))
	  (collect-usage-info-lambda form global-info local-info))))


(defun collect-usage-info-if (form global-info local-info)
  (foldl (la (sub-form glob-inf)
			 (collect-usage-info sub-form glob-inf local-info))
		 global-info
		 (cdr form)))

(defun collect-usage-info-cond (form global-info local-info)
  (foldl (la (sub-case global-info)
			 (foldl (la (sub-sub-case global-info)
						(collect-usage-info sub-sub-case global-info local-info))
					global-info
					sub-case))
		 global-info
		 form))

(defun defun-binds-list (arglist)
  (unique (filter 
		   (lambda (x)
			 (not (or
				   (eq x '&rest)
				   (eq x '&optional))))
		   arglist)))

(defun set-symbol-state (sym local-info state)
  (dlet [[bound-s bound-f] (symbol-binding-info sym local-info)]
	(alist>> local-info sym (list state bound-f))))

(defun set-function-state (sym local-info state)
  (dlet [[bound-s bound-f] (symbol-binding-info sym local-info)]
	(alist>> local-info sym (list bound-s state))))


(defun collect-usage-info-defun (form global-info local-info)
  (let* ((arglist (elt form 2))
		 (body (cdddr form))
		 (bound-symbols (defun-binds-list arglist))
		 (new-local-info 
		  (foldl (lambda (symbol-name new-local-info)
				   (set-symbol-state symbol-name new-local-info t)) local-info bound-symbols)))
	(collect-usage-info-prog-like body global-info new-local-info)))

(defun collect-usage-info-lambda (form global-info local-info)
  (let* ((arglist (elt form 1))
		 (body (cddr form))
		 (bound-symbols (defun-binds-list arglist))
		 (new-local-info 
		  (foldl (lambda (symbol-name new-local-info)
				   (set-symbol-state symbol-name new-local-info t)) local-info bound-symbols)))
	(collect-usage-info-prog-like body global-info new-local-info)))


(defun collect-usage-info-setq (form global-info local-info)
  (let* ((meat (cdr form))
		 (expressions 
		  (loop for subform in meat
				and i from 0 when (evenp subform)
				collect subform)))
	(collect-usage-info-prog-like expressions global-info local-info)))


(defun* collect-usage-info (form &optional (global-info (alist>>))
								 (local-info (alist>>)))
  "(collect-usage-info FORM &optional GLOBAL-INFO LOCAL-INFO)

Collects information about the usage of variables and functions in the form FORM.
When called recursively, it may be appropriate to provide GLOBAL-INFO, an alist 
describing the collected information so far, and LOCAL-INFO, an alist describing
the binding information of variables locally.  

GLOBAL-INFO is returned.  It is an alist of the form;
  (list 
    (<symbol> (<n-bound-symbol-usages>
               <n-bound-function-usages>
               <n-unbound-symbol-usages>
               <n-unbound-symbol-usages>))
    ...)

LOCAL-INFO is recursively defined, but is of the form
  (list 
    (<symbol> (<bound-as-symbol>
               <bound-as-function>))
    ...)

  Where bound-as-(function/symbol) are either t or nil."
  (cond
   ((numberp form) global-info)
   ((stringp form) global-info)
   ((vectorp form) global-info)
   ((symbolp form)
	(cond ((eq 't form) global-info)
		  ((eq 'nil form) global-info)
		  (t
		   (dlet [[bound-s bound-f] (symbol-binding-info form local-info)
				  [s-count f-count s-unbound-count f-unbound-count] (symbol-bind-counts form global-info)]
			 (alist>> global-info form (list 
										(if bound-s (+ 1 s-count) s-count)
										f-count
										(if (not bound-s) (+ 1 s-unbound-count) s-unbound-count)
										f-unbound-count))))))
   ((listp form)
	(cond 
	 ((quotep form)
	  global-info)
	 ((prog-like form)
	  (collect-usage-info-prog-like form global-info local-info))
	 ((letp form)
	  (collect-usage-info-let form global-info local-info))
	 ((let*p form)
	  (collect-usage-info-let* form global-info local-info))
	 ((function-formp form)
	  (collect-usage-info-function form global-info local-info))
	 ((ifp form)
	  (collect-usage-info-if form global-info local-info))
	 ((condp form)
	  (collect-usage-info-cond form global-info local-info))
	 ((defunp form)
	  (collect-usage-info-defun form global-info local-info))
	 ((lambdap form)
	  (collect-usage-info-lambda form global-info local-info))
	 ((setqp form)
	  (collect-usage-info-setq form global-info local-info))
	 (t ; function application
	  (collect-usage-info-prog-like `(progn ,@(cdr form))
									(collect-usage-info-function `(function ,(car form)) global-info local-info)
									local-info))))))

(defun get-unbound-symbols-list (global-info)
  (loop for element in global-info 
		when 
		(let* ((counts (cadr element))
			   (su-count (elt counts 2)))
		  ($ su-count > 0))
		collect (car element)))

(defun get-unbound-function-symbols-list (global-info)
  (loop for element in global-info 
		when 
		(let* ((counts (cadr element))
			   (fu-count (elt counts 3)))
		  ($ fu-count > 0))
		collect (car element)))

(defmacro* delay (&body body)
  (let* ((expanded (macroexpand-all `(progn ,@body)))
		 (info (collect-usage-info expanded))
		 (unbound-symbols (get-unbound-symbols-list info))
		 (unbound-functions (get-unbound-function-symbols-list info))
		 (temporary-function-names 
		  (loop for f in unbound-functions collect (gensym (format "%s-old-" f)))))
	`(lexical-let ,(loop for s in unbound-symbols collect `(,s ,s))
	   (labels ,(loop for old-f in temporary-function-names 
					  and f in unbound-functions collect 
					  (let ((arglist (gensym "arglist")))
						`(,old-f (&rest ,arglist) (apply #',f ,arglist))))
		 (labels ,(loop for f in unbound-functions 
						and old-f in temporary-function-names collect 
						(let ((arglist (gensym "arglist")))
						  `(,f (&rest ,arglist) (apply #',old-f ,arglist))))
		   (lambda () ,expanded))))))

(defun force (thunk)
  (funcall thunk))

(defun print-and-return (x)
  (print x)
  x)

(defmacro* capturing-lambda (args &body body)
  (let* ((expanded (cadr (macroexpand-all `(lambda ,args ,@body))))
		 (info (collect-usage-info expanded))
		 (unbound-symbols (get-unbound-symbols-list info))
		 (unbound-functions (get-unbound-function-symbols-list info))
		 (temporary-function-names  
		  (loop for f in unbound-functions collect (gensym (format "%s-old-" f)))))
	`(lexical-let ,(loop for s in unbound-symbols collect `(,s ,s))
	   (labels ,(loop for old-f in temporary-function-names 
					  and f in unbound-functions collect 
					  (let ((arglist (gensym "arglist")))
						`(,old-f (&rest ,arglist) (apply #',f ,arglist))))
		 (labels ,(loop for f in unbound-functions 
						and old-f in temporary-function-names collect 
						(let ((arglist (gensym "arglist")))
						  `(,f (&rest ,arglist) (apply #',old-f ,arglist))))
		   ,expanded)))))


(defmacro* capturing-defun (name args docstring &body body)
  (let* ((expanded (macroexpand-all `(defun ,name ,args ,docstring ,@body)))
		 (info (collect-usage-info expanded))
		 (unbound-symbols (get-unbound-symbols-list info))
		 (unbound-functions (get-unbound-function-symbols-list info))
		 (temporary-function-names  
		  (loop for f in unbound-functions collect (gensym (format "%s-old-" f)))))
	`(lexical-let ,(loop for s in unbound-symbols collect `(,s ,s))
	   (labels ,(loop for old-f in temporary-function-names 
					  and f in unbound-functions collect 
					  (let ((arglist (gensym "arglist")))
						`(,old-f (&rest ,arglist) (apply #',f ,arglist))))
		 (labels ,(loop for f in unbound-functions 
						and old-f in temporary-function-names collect 
						(let ((arglist (gensym "arglist")))
						  `(,f (&rest ,arglist) (apply #',old-f ,arglist))))
		   ,expanded)))))





(provide 'codewalking-utils)