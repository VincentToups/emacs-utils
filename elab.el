(defun nilp (x)
  (eq nil x))

(defun quotep (form)
  (and (listp form)
	   (not (nilp form))
	   (equal 'quote (car form))))

(setf *matlab-macros* (tbl!))
(defmacro* defmatlab-macro (name args &body body)
  (let ((actual-name (inter (format "%s-elab--" name))))
	`(progn
	   (defun* ,actual-name ,args ,@body)
	   (tbl! *matlab-macros* ',name ',actual-name))))


(defun matlab-macrop (symbol)
  ($ symbol in *matlab-macros*))

(defun expand-matlab-macro (form)
  (let ((macro-name (tbl *matlab-macros* (car form)))
		(macro-body (cdr form)))
	(apply macro-name body)))

(defun sym->camel-case (s)
  (let* ((parts (split-string (format "%s" s) "-"))
		 (parts (cons (car parts) (mapcar #'capitalize (cdr parts)))))
	(apply #'concat parts)))

(sym->camel-case 'test-this)

(defun prognp (form)
  (and (listp form)
	   (not (nilp form))
	   (equal (car form) 'progn)))

(defun elab-expand-progn (form)
  (let ((parts (mapcar #'form->matlab (cdr form))))
	(format "apply(@(varargin) varargin{end},%s)" 
			(join parts ", "))))

(elab-expand-progn '(progn 1 2 "a"))"apply(@(varargin) varargin{end},1, 2, 'a')"

(setq double-quote "\"")
(setq single-quote "'")

(defun form->matlab (form)
  (cond ((numberp form) (format "%s" form))
		((stringp form)
		 (concat single-quote (replace-regexp-in-string "'" "''" form) single-quote))
		((symbolp form) (sym->camel-case form))
		((listp form)
		 (cond 
		  ((nilp form) "[]")
		  ((quotep form) "'%s'" (replace-regexp-in-string "'" "''" (form->matlab (cadr form))))
		  ((matlab-macrop form)
		   (form->matlab (expand-matlab-macro form)))
		  (t 
		   (let ((f-name (sym->camel-case (car form)))
				 (args (join (mapcar #'form->matlab (cdr form)) ", ")))
			 (format "%s(%s)" f-name args)))))))


(form->matlab '(sin some-data 2))

