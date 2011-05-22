;;; Parser Bind

;;; So, we'd like a function which extracts the parser _value_ and
;;; binds it to a variable inside an expression which generates
;;; another parser.  That way we could use this function to construct
;;; nested, value-dependent parsers conveniently (or sort of
;;; conveniently).

;;; If this doesn't seem obvious, don't worry too much.  Once we do
;;; some examples, the utility will be clear.

;;; Consider:

(defun simple-parser-bind (parser parser-producer)
  (lexical-let ((parser parser)
				(parser-producer parser-producer))
	(lambda (input) ; we return a new parser
	  (unless (empty? input)
		(let* ((res (funcall parser input)))
			   (if res
				   (let ((new-parser (funcall parser-producer (parsed-value res))))
					 (funcall new-parser (parsed-leftover res)))
				 nil))))))

;;; In words: parser-bind takes 1 - a parser 2 - a function which
;;;  takes a value and returns a NEW parser, which may *depend* on
;;;  that value.  
;;; It returns a parser itself.
;;; This returned parser:
;;;  1 - applies PARSER to its input, generating a value/leftover pair.
;;;  2 - extracts the VALUE part of that pair, and creates yet another
;;;      parser by calling PARSER-PRODUCER on that value.
;;;  3 - finally, it applies this new parser to the leftovers from PARSER

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index