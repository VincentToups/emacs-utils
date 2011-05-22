;;; Useful Combinators

(defun -satisfies (pred)
  (lexical-let ((pred pred))
	(parser-let* 
	 ((item #'anything))
	 (if (funcall pred item) 
		 (simple-parser-return item)
	   #'nil-parser))))

(defun -manythings (n)
  (lexical-let ((n n))
	(lambda (input)
	  (if (< (length input) n) nil
		(pair 
		 (substring input 0 n)
		 (substring input (min (length input) n)))))))

(defun -matches (str)
  (lexical-let ((str str)) ; parser-let* implicitely 
                          ; constructs a function
                          ; which requires str
			  (parser-let*
			   ((sub (-manythings (length str))))
			   (if (string= sub str)
				   (simple-parser-return sub)
				 #'nil-parser))))

;;; because of the behavior of bind, we can't write the following
;;; function with parser-let*:

(require 'recur)
(defun -or (&rest parsers)
  (lexical-let ((parsers parsers))
	(lambda (input)
	  (unless (empty? input)
		(recur-let 
		 ((rem-parsers parsers))
		 (cond
		  ((empty? rem-parsers) nil)
		  (t 
		   (let ((r (funcall (car rem-parsers) input)))
			 (if r r
			   (recur (cdr rem-parsers)))))))))))
  
;;; example:

(defun -cat-or-dog ()
  (parser-let* ((res (-or (-matches "cat")
						  (-matches "dog"))))
			   (simple-parser-return res)))

(funcall (-cat-or-dog) "ewe")
(funcall (-cat-or-dog) "cat")
(funcall (-cat-or-dog) "dog")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index