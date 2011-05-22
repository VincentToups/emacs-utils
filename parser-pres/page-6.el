;;; Limitations of combine-parsers 

;;; Combine-parsers works ok when we want to combine two parsers.  We
;;; can even use the with argument to shoehorn more parsers together.

(defun parse-c (input)
  (unless (empty? input)
	(if (string= (str-head input) "c")
		(pair :found-c (str-tail input))
	  nil)))

(defun parse-a-b-c (input)
  (funcall
   (combine-parsers 
	(combine-parsers #'parse-a #'parse-b) 
	#'parse-c #'suffix)
   input))

(parse-a-b-c "abcdef")

;;; But that is really pretty inconvenient.  And if we want to combine
;;; parsers which depend on the results of previous parsings,
;;; "combine-parsers" won't cut it.

;;; The crux of the issue is that we are really interested in the
;;; VALUE our parsers return, when combining parsers.  We need an
;;; interface to expose these values selectively.  

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index