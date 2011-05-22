;;; More Combinators 

(defun -zero-or-more (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(recur-let ((result (funcall parser input))
					(acc nil)
					(last-input input))
				   (if result
					   (recur
						(funcall parser (parsed-leftover result))
						(cons (parsed-value result) acc)
						(parsed-leftover result))
					 (pair (reverse acc)
						   last-input)))))))

(funcall (-zero-or-more 
		  (-matches "a"))
		 "aaaab")
(funcall (-zero-or-more 
		  (-matches "a"))
		 "bbbb")

(defun -one-or-more (parser)
  (lexical-let ((parser parser))
	(parser-let* ((first parser)
				  (rest (-zero-or-more parser)))
				 (simple-parser-return (cons first rest)))))

(funcall (-one-or-more
		  (-matches "dog "))
		 "dog dog dog dog cat")

(funcall (-one-or-more 
		  (-matches "dog "))
		 "cat dog dog dog cat")

(defun -maybe (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(let ((r (funcall parser input)))
		  (if r r
			(pair nil input)))))))

(defun pempty? (input)
  "Check to see if you have hit the end of the input."
  (if (empty? input) (pair t input)
	(pair nil input)))

(defun -list (parser)
  (lexical-let ((parser parser))
	(parser-let* ((r parser))
				 (simple-parser-return 
				  (list r)))))

(defun -not (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(let ((r (funcall parser input)))
		  (if r nil
			(pair t input)))))))

(defun -and2 (p1 p2)
  (lexical-let ((p1 p1)
				(p2 p2))
	(parser-let* ((v1 p1)
				  (v2 p2))
				 (simple-parser-return v2))))

(defun -and (&rest ps)
  (reduce #'-and2 ps))

(defun -and-list (&rest ps)
  (lexical-let ((ps ps))
	(if (empty? ps)
		(lambda (input)
		  (pair nil input))
	  (parser-let* 
	   ((v (car ps))
		(rest (apply #'-and-list (cdr ps))))
	   (simple-parser-return (cons v rest))))))

(defun -n-of (n parser)
  (if (= n 1) (-list parser)
	(lexical-let ((n n)
				  (parser parser))
	  (parser-let* 
	   ((head parser)
		(rest (-n-of (- n 1) parser)))
	   (simple-parser-return (cons head rest))))))

(funcall (-n-of 3 (-matches "a")) "aaab")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index