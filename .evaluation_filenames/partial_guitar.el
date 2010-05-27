(defun take-randomly (lst)
  (let ((ind (floor (random (length lst)))))
	(dloop [index 0
		    [item & rest] lst
			front nil]
		   (if (= index ind) (list item (append (reverse front) rest))
			 (recur (+ 1 index) rest (cons item front))))))

(take-randomly '(1 2 3 4 5 6))

(defun generate-plucking-pattern ()
  (dloop [in-strings '(E A D G B e)
         out-strings nil]
	(if in-strings
		(dlet [[item rest] (take-randomly in-strings)]
		  (recur rest (cons item out-strings)))
	  out-strings)))

(generate-plucking-pattern)

