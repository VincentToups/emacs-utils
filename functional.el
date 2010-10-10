
(defmacro defcurryl (newname oldname &rest args)
  (let ((narglist (gensym (format "%s-arglist" newname))))
	`(defun ,newname (&rest ,narglist)
	   (apply #',oldname ,@args ,narglist))))

(defmacro defcurryr (newname oldname &rest args)
  (let ((narglist (gensym (format "%s-arglist" newname))))
	`(defun ,newname (&rest ,narglist)
	   (apply #',oldname (append ,narglist (list ,@args))))))

(defmacro clambdal (oldf &rest args)
  (let ((narglist (gensym "clambdal-arglist-")))
	`(lambda (&rest ,narglist)
	   (apply #',oldf ,@args ,narglist))))

(defmacro clambdar (oldf &rest args)
  (let ((narglist (gensym "clambdal-arglist-")))
	`(lambda (&rest ,narglist)
	   (apply #',oldf (apply ,narglist (list ,@args))))))

(defmacro defdecorated (newname oldname transformer)
  (let ((args (gensym (format "%s-decorated-args" newname))))
	`(defun ,newname (&rest ,args)
	   (apply #',oldname 
			  (funcall #',transformer ,args)))))

(defmacro lambdecorate (oldf transformer)
  (let ((args (gensym (format "decorated-args"))))
	`(lambda (&rest ,args)
	   (apply #',oldf
			  (funcall #',transformer ,args)))))

(provide 'functional)
