(require 'functional)
(require 'monad-parse)
(provide 'sqlite)

(setq *sqlite-bin* "/usr/bin/sqlite3")

(defmacro declare-literal-symbol-parsers (&rest symbols)
  `(progn
	 ,@(loop for s in symbols collect
			 `(defun ,(internf "=lit-%s" s) ()
				(=lit-sym ',s)))
	 ',(mapcar
		(pal #'internf "=lit-%s") symbols)))

(declare-literal-symbol-parsers 
 insert 
 replace
 or
 rollback
 abort
 replace
 fail
 ignore
 into
 default
 values)

(defun =insert-modifier ()
  (=let* [_ (=lit-or) 
		  modifier 
		  (=or 
		   (=lit-rollback)
		   (=lit-abort)
		   (=lit-replace)
		   (=lit-fail)
		   (=lit-ignore))]
		 modifier))

(defun =parse-database-info ()
  (=let* [res (=or 
   (=satisfies #'stringp)
   (=and (=satisfies #'listp)
		 (=satisfies (comp #'stringp #'car))
		 (=satisfies (comp #'stringp #'cdr))))]
		 (if res 
			 (if (listp res)
				 (alist>> :name (car res)
						  :table (cdr res)))
		   res)))

(defun =default-values-seq ()
  (=let* [def (=satisfies (par 'eq 'default))
			  val (=satisfies (par 'eq 'values))]
		 'default-values))

(defun =parse-name-values ()
  (=let* [names (=satisfies
				 (lambda (thing)
				   (and (listp thing)
						(and-over #'symbolp thing))))
				sigil (=lit-values)
				values (=satisfies 
						(lambda (thing)
						  (listp thing)
						  (= (length thing) (length names))))]
		 (alist>> :names names :values values)))

(defun =parse-tail-info ()
  (=or (=default-values-seq)
	   (=parse-name-values)))

(defun parse-insert-statement ()
  (=let* [form-type (=or 
					 (=lit-replace)
					 (=lit-insert))
					modifier (=maybe (=insert-modifier))
					_ (=lit-into)
					database-info (=parse-database-info)
					tail (=parse-tail-info)]
		 (alist>> :form-type form-type
				  :modifier modifier
				  :database-info database-info
				  :tail tail)))


(parse-sequence (parse-insert-statement) '(insert into ("database" . "table")  default values))



(defun escape-quote (s)
  (replace-regexp-in-string (rxq "'")
							"''" s))

(defun sqlify (datum)
  (cond 
   ((stringp datum) 
	(format "'%s'" (escape-quote datum)))
   ((numberp datum)
	(format "%d" datum))
   ((keywordp datum)
	(format "'(make-keyword \"%s\")'" datum))
   ((symbolp datum)
	(format "'(intern \"%s\")'" datum))
   ((listp datum)
	(format "'%S'" (escape-quote (format "%S" datum))))))

(defun sql-create (table 

(defun atomic-insert (database-file
					  table
					  fields
					  values)
  (let* ((statement 
		  (format "insert into %s (%s) values (%s);\n"
				  table
				  (join (mapcar (cl #'format "%s") 
								fields) ", ")
				  (join (mapcar #'sqlify values) ", ")))
		 (file-name (cadr 
					 (with-write-temp-file 
					  (insert statement)))))
	(prog1 (with-temp-buffer 
			 (call-process *sqlite-bin* file-name (current-buffer) nil database-file)
			 (buffer-substring (point-min) (point-max)))
	  (delete-file file-name))))





