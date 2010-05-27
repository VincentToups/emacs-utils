(require 'macro-utils)

(defun dec-name (form) (third form))
(defun dec-spec (form) (second form))

(defun parse-type-expression (form acc)
  (cond ((symbolp form) (concat (format "%s " form) acc))
		((vectorp form) 
		 (concat (join (mapcar (lambda (x) (format "%s" x)) (coerce form 'list)) " ") " "acc))
		((listp form)
		 (if (< (length form) 2) (error (format "Can't understand form %s." form))
		   (let ((type (car form)))
			 (case type 
			   ('array-of 
				(parse-type-expression
				 (cadr form)
				 (concat "(" acc ")"
						 (format "[%s]"
								 (if (> (length form) 2) (elt form 2) "")))))
			   ('function-returning
				(parse-type-expression (cadr form) (format acc "(%s())")))
			   ('pointer-to 
				(parse-type-expression (cadr form) (concat "*(" acc ")")))
			   (otherwise (error (format "Can't understand form %s." form)))))))))

(defun emit-dec (form)
  (let ((name (dec-name form))
		(type-expression (dec-spec form)))
	(concat (format (parse-type-expression type-expression "%s") name) ";\n")))

(defun literallyp (form)
  (and (non-empty-listp form)
	   (eq (car form) 'literally)))

(setf *elcie-macros* (tbl!))

(defmacro defmacro-elcie (name args &rest body)
  `(progn 
	 (defun ,(internf "elcie-macro-%s" name) ,args
	   ,@body)
	 (tbl! *elcie-macros* ',name #',(internf "elcie-macro-%s" name))))

(defun elcie-macrop (form)
  (and (non-empty-listp form)
	   (in (car form) *elcie-macros*)))

(defun elcie-macro-expand (form)
  (let ((macrof (tbl *elcie-macros* (car form))))
	(apply macrof (cdr form))))

(defun emit-elcie-macro (form)
  (compile-elci (elcie-macro-expand form)))

(defun emit-elci-function-call (form)
  (let ((fname (car form))
		(args (cdr form)))
	(format "%s(%s)" fname
			(join (mapcar #'compile-elci args) ","))))

(defmacro-elcie progn (&rest body)
  `(literally ,(apply #'concat 
					  (mapcar #'compile-elci body))))

(defmacro-elcie block (&rest body)
  `(literally ,(concat "{\n" (apply #'concat 
									(mapcar #'compile-elci body)) "\n}\n")))

(dont-do
 (defmacro-elcie dec (type name)
   `(literally ,(emit-dec `(dec ,type ,name))))

 (elcie-macrop '(dec int x))
 (elcie-macro-expand '(dec int x))
 (emit-elcie-macro '(dec int x))

 (emit-elcie-macro '(block (dec (pointer-to int) x) (dec int y)))

 )

(defun compile-elci (form)
  (cond
   ((numberp form)
	(format "%s" form))
   ((stringp form)
	(format "\"%s\"" form))
   ((symbolp form)
	(emit-symbol form))
   ((listp form)
	(cond
	 ((literallyp form)
	  (if (< (length form) 2) ""
		(cadr form)))
	 ((elcie-macrop form)
	  (emit-elcie-macro form))
	 (t 
	  (emit-elci-function-call form))))
   ((vectorp form)
	(emit-indexing form))))

(dont-do 
 ((setp form)
  (emit-set form))
 ((defp form)
  (emit-def form))
 ((ifp form)
  (emit-if form))
 ((forp form)
  (emit-for form))
 ((switchp form)
  (emit-switch form)))