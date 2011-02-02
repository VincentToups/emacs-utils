(require 'monad-parse)

(lex-defun =string-flat (str)
		   (lexical-let ((lst-str (coerce str 'list)))
			 (foldl 
			  (lex-lambda (char parser-acc)
						  (=and parser-acc (=char char)))
			  (=char (car lst-str))
			  (cdr lst-str))))

(lex-defun =string-flat (str)
		   (apply #'=and-concat (mapcar #'=char->string (coerce str 'list))))

(loop for (a . b) in '( (hey . yeah) (yo . uh) ) collect (list a b))

(funcall (parser-items->string (length "test")) (->in "test a test"))
(funcall (parser-item) (->in "test"))

(funcall (=string "test a test") (->in "test a test"))

(string= nil "test")

(funcall (zero-or-more (=char->string ?a)) (->in "aaab"))

(funcall (parser-bind 
		  (zero-or-more (=char->string ?a))
		  (lex-lambda (value) (parser-return (apply #'concat value)))) (->in "aaab"))

(lex-defun parser-concat (parser)
		   (parser-bind parser
						(lex-lambda (value) (parser-return (apply #'concat value)))))

(equal (parser-concat (=char->string ?a))
	   (parser-bind 
		(zero-or-more (=char->string ?a))
		(lex-lambda (value) (parser-return (apply #'concat value)))))
(funcall (parser-concat (=char->string ?a))
	   (->in "aaab"))


						 (lex-defun parser-apply (parser f)
									(parser-bind 
									 parser
									 (lex-lambda (value)
												 (parser-return (apply f value)))))


(parser-apply (funcall (zero-or-one (=char->string ?a)) (->in "aaab")) #'concat)