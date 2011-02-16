(provide 'classy)
(require 'multimethods)

(defvar *classy-weak-table* (make-hash-table :test 'eq :weakness t) "Classy table to distinguish between lists and instances.")

(defun classy-alist>> (&rest args)
  (let ((o (apply #'alist>> args)))
	(alist! o :--classy-tag (gensym "classy-tag-"))
	(alist! o :--class :thing)
	 (setf (gethash (alist o :--classy-tag) *classy-weak-table*) t)
	o))

(let ((cc (classy-alist>> :x 10 :y 11)))
  (gethash (alist cc :--classy-tag) *classy-weak-table*)
  (alist cc :--classy-tag))

(defun classy-objectp (object)
  (and (listp object)
	   (gethash (alist object :--classy-tag) *classy-weak-table*)))

k			  
