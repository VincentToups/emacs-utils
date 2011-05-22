;;; Meditation Upon Combination

;;; parse-ab was a mess.  Can we factor out this complexity?

(defun* combine-parsers (p1 p2 &optional (with #'list))
  (lexical-let ((p1 p1)
				(p2 p2)
				(with with)) ; create lexical copies of p1 and p2
										; since we are returning a lambda that
										; which depends on them.
	(lambda (input) 
	  (unless (empty? input)
		(let ((r1 (funcall p1 input)))
		  (if r1 
			  (let* ((v1 (parsed-value r1))
					 (leftover1 (parsed-leftover r1))
					 (r2 (funcall p2 leftover1)))
				(if r2 
					(pair (funcall with v1
								   (parsed-value r2))
						  (parsed-leftover r2))))))))))

;;; COMBINE-PARSERS is a *combinator* or higher order function in the
;;; functional-programming sense.  It is a function which operates on
;;; functions and returns a new function.

(funcall (combine-parsers #'parse-a #'parse-b) "abraham a)")

;;; pretty sweet!

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index