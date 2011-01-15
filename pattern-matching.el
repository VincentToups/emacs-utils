
(provide 'pattern-matching)

(do-patterns (list 1 2 3)
			 (cons first rest) (list first rest)) -> (1 (2 3))

(let* ((val (list 1 2 3))
	   (cons-pattern-result (cons-pattern '(first rest) val)))
  (cond (cons-pattern-result (let ((first (elt cons-pattern-result 0))
								   (second (elt cons-pattern-result 1)))

(defmacro do-patterns 