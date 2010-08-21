(setf fact-db (cl-make-hash-table :test 'equal))

(defmacro fact (name args &rest terms)
  `(setf (tbl fact-db '(,name ,@args))
		 ,(if terms `(quote ,terms) t)))

(defun capitilized-symbol? (sym)
  (let* ((str-version (format "%s" sym))
		 (first-char (substring str-version 0 1)))
	(string= first-char (upcase first-char))))

(defun prolog-constant? (sym)
  (or (numberp sym)
	  (not (capitilized-symbol? sym))
	  (stringp sym)))

(defun constant-form? (form)
  (and-over #'prolog-constant? (cdr form)))
(defmacro query (&rest form)
  (cond 
   ((constant-form? form) `($ ',form in fact-db 'equal))
   (t 'too-dumb-for-this-query)))

(macroexpand '(fact father (vincent) (child-of X vincent) (married vincent (mother-of Y X))))

(fact human (vincent))
(fact human (shelley))
(fact mother (shelley weather))
(fact father (vincent weather))
(fact human (X) (mother Y X) (father Z X) (human Y) (human Z))
(keyshash fact-db)
(query human vincent)
(macroexpand '(query human vincent))
(in '(human vincent) (keyshash fact-db) 'equal)
(equal '(human vincent) '(human vincent))

(fact parent (vincent weather))
(fact parent (shelley weather))
(fact parent (shelley flint))
(fact parent (vincent flint))
(fact parent (debbie sophie))
(fact parent (brian sophie))


(query parent X Y)
; find possible values of X, these are any facts or rules in the data base for which (parent X Y) could possibly be true.  
; find possible values of Y, these are any facts or rules for which (parent x Y) is true, where x [= X

(parent 
