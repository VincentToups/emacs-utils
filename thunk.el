(require 'macro-utils)

(defmacro* thunk (&body body)
  (let* ((syms (unique (filter #'symbolp (flatten body))))
		 (free-syms 
		  (filter 
		   (lambda (x) 
			 ($ (count-free-usages x (cons 'progn body)) > 0))
		   syms)))
	`(lexical-let ,(loop for s in free-syms collect (list s s))
	   (lambda () ,@body))))

(setf test (let ((x 10))
			 (thunk (+ x x))))

  (defun force (thunk)
	(funcall thunk))

  (force test)

  (macroexpand '(thunk (+ x x)))
(count-free-usages '+ '(+ x x))