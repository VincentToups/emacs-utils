;;; Demystifying the Macro Magic

;;; consider that :

(let* ((x 10)
	   (y 11))
  (+ x y))

(let* ((x 10)
	   (y (+ x 1)))
  (+ x y))

;;; expands to
(comment
 (funcall
  (lambda (x) 
   (funcall (lambda (y) (+ x y)) 11)) 
  10)
)

;;; or, provacatively:
(comment 
(defun id-bind (v f)
  (funcall f v))

(id-bind 
 10 
 (lambda (x)
   (id-bind 
	11 
	(lambda (y) 
	  (+ x y))))))

;;; or the semantic equivalent.
;;;
;;; parser-let*, then:

(parser-let* 
 ((a #'parse-a)
  (b #'parse-b))
 (simple-parser-return 
  (list a b)))

;;; expands to:

(comment

(parser-bind 
 #'parse-a 
 (lambda (a) 
   (parser-bind 
	#'parse-b 
	(lambda (b) 
	  (simple-parser-return
	   (list a b))))))
)

;;; parser-let* is a generalization of let* which knows about how we
;;; want to combine parsers.  Monads in general support extension of
;;; the idea of let*. That is, sequencing dependent computations.
							   
							   
;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index