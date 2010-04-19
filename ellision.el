(defun let-like? (form)
  (and (listp form)
	   (let ((head (first form)))
		 (foldl 
		  (lambda (it ac)
			(or (= it head)))
		  nil
		  '(let let* lexical-let lexical-let*)))))
(defun expand-ellision (form)
  (cond 
   ((atom form) form)
   ((listp form)
	(let* ((head (first form))
		   (head-string (format "%s" head))
		   (any-&

(defmacro* with-ellision (&body body)
  (let ((exp-body (macroexpand-all body)))
	

(macroexpand-all '(defun f (x) (+ x 1)))

