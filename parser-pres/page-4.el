;;; Combining Parsers

;;; Our goal is to make writing parsers as easy as writing programs.
;;; We program by combining simple functions.  How do we combine
;;; simple parsers?

(defun parse-b (input)
  (unless (empty? input)
	(if (string= (str-head input) "b")
		(pair :found-b (str-tail input)))))

(defun parse-ab (input)
  (unless (empty? input)
	(let ((a-result (parse-a input)))
	  (if a-result 
		  (let* ((a-val (parsed-value a-result))
				 (new-input (parsed-leftover a-result))
				 (b-result (parse-b new-input)))
			(if b-result
				(let* ((b-val (parsed-value b-result)))
				  (pair (list a-val b-val) 
						(parsed-leftover b-result)))))))))

(parse-ab "abracadabra")
(parse-ab "atrophy")
(parse-ab "oboe")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index