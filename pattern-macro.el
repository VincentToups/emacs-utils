(require 'monads)
(require 'monad-parse)

(lex-defun item->parser-function (literals item)
  (cond
   ((symbolp item)
	(if ($ item in literals) 
		(lexical-mlet monad-parse ((_ (=satisfies (par #'eq item))))
				 (m-return (list (list item _))))
	  (lexical-mlet monad-parse 
			   ((thing (=satisfies (always t))))
			   (m-return
				(list (list item thing))))))))

(parse-sequence (item->parser-function '(x) 'x) '(x))
(setq p (parser-append (item->parser-function '(x) 'x)
					   (item->parser-function '(x) 'y)))

(parse-sequence p '(x :hat))

(with-monad-dyn monad-parse
				(parse-sequence p '(x :hat)))



(lex-defun parser-append (p1 p2)
  (lexical-mlet monad-parse
		   ((v1 p1)
			(v2 p2))
		  (m-return (append v1 v2))))

(with-monad-dyn monad-parse
				(m-chain
				 (item->parser-function '(z) 'x)
				 (item->parser-function '(x) 'x)))


(defun pattern->parser (literals pattern)
  (with-monad-dyn monad-parse
				  (m-chain (mapcar (pal #'item->parser-function literals) pattern))))

(parse-sequence (funcall (pattern->parser '(x) '(a b x c))) '(q r x b))

(with-monad-dyn monad-parse
				(parse-sequence (pattern->parser '(x y) '(a b x z)) '(10 11 'x 'hey)))
			 

(defmacro pattern-macro (literals &rest {pattern/expansion})

  