(provide 'classy)
(require 'multi-methods)
(require 'cl)
(require 'utils)

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

(defun classy-dispatch-single (object)
  (cond ((listp   object) 
		 (cond 
		  ((classy-objectp object) (alist object :--class))
		  (t :list)))
		((numberp object) :number)
		((bufferp object) :buffer)
		((vectorp object) :vector)
		((stringp object) :string)
		((hash-table-p object) :hash-table)
		((functionp object) :function)
		((keywordp object) :keyword)
		((symbolp object) :symbol)
		(t (error "Can't find a classy type for %S" object))))

(defun classy-dispatch (&rest args)
  (map 'vector #'classy-dispatch-single args))

										; hierarchy

($ [:number :buffer :function :keyword :symbol :collection :classy-class] derive-from :thing)
($ [:list :vector :hash-table] derive-from :collection)

(defvar *classy-classes* (make-hash-table :test 'eq))

(defun make-classy-class (&rest class-name parents field-thunk-pairs)
  (let ((class (classy-alist>> 
	  (loop for p in (coerce parents 'list) do
			($ class-name derives-from p))
	(

