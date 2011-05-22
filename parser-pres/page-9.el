;;; Oh Dang it is the Lisp Slide

;;; All this junk about bind will melt into the background once we
;;; have one nice piece of syntax.  

;;; We are about to roll a parser-specific equivalent of Haskell's do
;;; notation.  If you don't care about lisp, feel free to tune this
;;; out.

(defmacro parser-let* (binding-forms &rest body)
  (if (empty? binding-forms) `(progn ,@body)
	(let* ((binding-form (car binding-forms))
		   (subsequent-binders (cdr binding-forms))
		   (symbol (car binding-form))
		   (expression (cadr binding-form)))
	  `(simple-parser-bind ,expression
						   (lex-lambda (,symbol)
									   (parser-let* ,subsequent-binders ,@body))))))

;;; (lex-lambda creates a lexical closure around its arguments,
;;; otherwise it is a simple lambda expression.

(defun simple-parser-return (val)
  (lexical-let ((val val))
	(lambda (input)
		(pair val input))))

(find-file-other-frame "~/work/art/monadic-return.png")

(funcall (parser-let* 
		  ((a-res #'parse-a)
		   (b-res #'parse-b)
		   (c-res #'parse-c))
		  (simple-parser-return (list a-res b-res c-res)))
		 "abcdef")

;;; ZING!

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index