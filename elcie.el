(require 'macro-utils)

(defun dec-name (form) (third form))
(defun dec-spec (form) (second form))



(defun emit-binary-operator (op args)
  (concat "(("
		  (join 
		   (mapcar #'compile-elci args)
		   (format ")%s(" op)) "))"))

(dont-do 
 (emit-binary-operator '+ (list 1 2 3 4)))

(defun* parse-type-expression (form &optional (acc "%s"))
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
	(concat (format (parse-type-expression type-expression "%s") name) "")))

(defun emit-symbol (form)
  (format "%s" form))

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

(defmacro-elcie typedef (type alias)
  `(literally ,(concat "typedef " (format (parse-type-expression form) "") " " (format "%s" alias))))

(defmacro-elcie include<> (&rest files)
  `(literally ,(concat (join (mapcar
							  (lambda (x)
								(format "#include<%s>" x))
							  files) (format "\n")) (format "\n"))))

(defmacro-elcie include (&rest files)
  `(literally ,(concat (join (mapcar
							  (lambda (x)
								(format "#include\"%s\"" x))
							  files) (format "\n")) (format "\n"))))

(defun add-semi-if-not-block (s)
  (if (string-represents-blockp s) s
	(concat s ";")))

(defmacro-elcie if (pred true-body &optional false-body)
  `(literally ,(format "if(%s) %s %s"
					   (compile-elci pred)
					   (add-semi-if-not-block (compile-elci true-body))
					   (if false-body (concat "else " (add-semi-if-not-block (compile-elci false-body))) ""))))

(defun emit-newline ()
  (format "\n"))

(defmacro-elcie block (&rest body)
  `(literally ,(concat "{" (emit-newline) (join
										   (loop for form in body collect 
												 (let ((cc (compile-elci form)))
												   (if (string-represents-blockp cc)
													   cc
													 (concat cc ";"))))
										   (emit-newline)) (emit-newline) "}" (emit-newline))))


(defmacro-elcie fun (out-type name in-types &rest body)
  `(literally ,(format "%s %s(%s){\n %s\n }"
					   (format (parse-type-expression out-type) "")
					   name
					   (join (mapcar (lambda (x) (format (parse-type-expression (car x)) (cadr x)))
									 in-types) ",")
					   (compile-elci `(block ,@body)))))

(defmacro-elcie return (val-expr)
  `(literally ,(format "return %s" (compile-elci val-expr))))

(defun last-character (s)
  (let ((n (length s)))
	(substring s (- n 1) n)))

(defun string-represents-blockp (s)
  (string= (last-character (chomp s)) "}"))

(defmacro-elcie dec (type name)
  `(literally ,(emit-dec `(dec ,type ,name))))

(setf c-binary-operators '(+ - / * & && | || < > == <= >= !=))

(loop for op in c-binary-operators do
	  (eval `(defmacro-elcie ,op (&rest rest)
			   `(literally ,(emit-binary-operator ',op rest)))))

(dont-do

 (emit-elcie-macro '(include<> stdio.h stdlib.h math.h))
 (emit-elcie-macro '(include a.h b.h c.h))

 (emit-elcie-macro '(- 1 2 3))
 

 (elcie-macrop '(dec int x))
 (elcie-macro-expand '(dec int x))
 (emit-elcie-macro '(dec int x))

 (emit-elcie-macro '(fun int main ((int argc) ((pointer-to (pointer-to char)) argv)) (return (+ argc argc))))

 (emit-elcie-macro '(block (dec (pointer-to int) x) (dec int y)))
 (emit-elcie-macro '(block (dec int x) (dec int y) (dec int z) (if (greater-than x 10) (block (plus x y)) (block (minus x y)))))


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