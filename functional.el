(require 'codewalking-utils)
(require 'utils)
(require 'cl)

(defmacro defcompose (name &rest fs)
  "Defun a new function NAME as a composition of other functions FS.  If (car FS) is a string, use this as the doc string."
  (let ((args (gensym (format "%s-args" name))))
	(if (stringp (car fs))
		`(defun ,name (&rest ,args) ,(car fs)
		   (apply (comp ,@(cdr fs)) ,args))
	  `(defun ,name (&rest ,args)
		 (apply (comp ,@fs) ,args)))))

(defun /|-argpred (x)
  (and (symbolp x)
	   (let* ((strv (format "%s" x))
			  (first-char (substring strv 0 1))
			  (rest-chars (substring strv 1 (length strv)))
			  (rest-count (string-to-number rest-chars)))
		 (and (string= "%" first-char)
			  (> rest-count 0)))))

(defun arg-num (arg)
  (let* ((strv (format "%s" arg))
		 (first-char (substring strv 0 1))
		 (rest-chars (substring strv 1 (length strv)))
		 (rest-count (string-to-number rest-chars)))
	rest-count))

(defun arg< (arg1 arg2)
  (< (arg-num arg1) (arg-num arg2)))

(defmacro* /| (&body body)
  (let* ((expanded (macroexpand-all `(progn ,@body)))
		 (usage-info (collect-usage-info expanded))
		 (args (filter #'/|-argpred (get-unbound-symbols-list usage-info)))
		 (args (functional-sort args #'arg<)))
	`(function (lambda ,args ,expanded))))

(defmacro defcurrly-doc (newname doc f &rest args)
  "Define a function by left-most partial application with doc string."
  (let ((narglist (gensym (format "%s-arglist" newname))))
	`(defun ,newname (&rest ,narglist)
	   ,doc
	   (apply ,f ,@args ,narglist))))

(defmacro defcurryl-no-doc (newname f &rest args)
  "Define a function by left-most partial application without doc string."
  (let ((narglist (gensym (format "%s-arglist" newname))))
	`(defun ,newname (&rest ,narglist)
	   (apply ,f ,@args ,narglist))))

(defmacro defcurryl (newname &rest args)
  "Define a function with left-most partial application on another function."
  `(defcurryl-no-doc ,newname ,@args))
;; (if (stringp (car args))
;; 	  `(defcurryl-doc ,newname ,(car args) ,@(cdr args))
;; 	`(defcurryl-no-doc ,newname ,@args)))

(defmacro defcurryr (newname oldname &rest args)
  (let ((narglist (gensym (format "%s-arglist" newname))))
	`(defun ,newname (&rest ,narglist)
	   (apply ,oldname (append ,narglist (list ,@args))))))

(defmacro clambdal (oldf &rest args)
  (let ((narglist (gensym "clambdal-arglist-")))
	`(lambda (&rest ,narglist)
	   (apply ,oldf ,@args ,narglist))))

(defmacro cl (&rest stuff)
  `(clambdal ,@stuff))


(defmacro clambdar (oldf &rest args)
  (let ((narglist (gensym "clambdal-arglist-")))
	`(lambda (&rest ,narglist)
	   (apply ,oldf (append ,narglist (list ,@args))))))

(defmacro cr (&rest stuff)
  `(clambdar ,@stuff))

(defun par (f &rest partially-applied-args)
  (lexical-let ((f f)
				(partially-applied-args partially-applied-args))
	(lambda (&rest unapplied)
	  (apply f (append unapplied partially-applied-args)))))

(defun pal (f &rest partially-applied-args)
  (lexical-let ((f f)
				(partially-applied-args partially-applied-args))
	(lambda (&rest unapplied)
	  (apply f (append partially-applied-args unapplied)))))

(defmacro defdecorated (newname oldname transformer)
  (let ((args (gensym (format "%s-decorated-args" newname))))
	`(defun ,newname (&rest ,args)
	   (apply ,oldname 
			  (funcall ,transformer ,args)))))

  (defmacro lambdecorate (oldf transformer)
	(let ((args (gensym (format "decorated-args"))))
	  `(lambda (&rest ,args)
		 (apply #',oldf
				(funcall #',transformer ,args)))))

  (lex-defun f-and-2 (f1 f2)
			 (lambda (&rest args) 
			   (and (apply f1 args)
					(apply f2 args))))

  (lex-defun f-and (&rest fs)
			 (reduce #'f-and-2 fs))

  (lex-defun f-or-2 (f1 f2)
			 (lambda (&rest args)
			   (or (apply f1 args)
				   (apply f2 args))))

  (lex-defun f-or (&rest fs)
			 (reduce #'f-or-2 fs))

  (lex-defun f-not (f)
			 (lambda (&rest args)
			   (not (apply f args))))

  (lex-defun f-mapcar (f)
			 (lambda (&rest args)
			   (apply #'mapcar (cons f args))))

  (lex-defun decorate-all (f dec)
			 (lambda (&rest args)
			   (apply f (mapcar dec args))))

(lex-defun decorate-n (f index trans)
  (lambda (&rest args)
	(let* ((el (elt args index))
		   (new (funcall trans el)))
	  (setf (elt args index) new)
	  (apply f args))))

(lex-defun f-comb-with (f-comb f1 f2)
  (lambda (&rest args)
	(funcall f-comb (apply f1 args) (apply f2 args))))
  

(provide 'functional)
