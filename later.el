(require 'cl)
(require 'utils)

(eval-when-compile-also
 (defun single-symbol-list? (item)
   (and (listp item)
		(= (length item) 1)
		(symbolp (car item))))
 (defun binderish? (item)
   (and (listp item)
		(= (length item) 2)
		(symbolp (car item))))

 (defun with-form->binder (item)
   (cond ((symbolp item )(list item item))
		 ((listp item)
		  (cond ((single-symbol-list? item)
				 (cons (car item) item))
				((binderish? item)
				 item)
				(t (error "with-forms require symbols, a single symbol list, or a binder-like expression.  Got %S." item))))
		 (t (error "with-forms require symbols, a single symbol list, or a binder-like expression.  Got %S." item))))

 (defmacro* later (expr &key (with nil) (with* nil))
   (cond (with 
		  `(lexical-let ,(mapcar #'with-form->binder with)
			 (later ,expr :with* ,with*)))
		 (with* 
		  `(lexical-let* ,(mapcar #'with-form->binder with*)
			 (later ,expr)))
		 (t `(lambda () ,expr)))))

(provide 'later)
