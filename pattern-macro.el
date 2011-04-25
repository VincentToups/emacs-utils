(require 'monads)
(require 'monad-parse)

(lex-defun =symbol-or-lit (literals)
  (lexical-let ((literals literals))
	(=or
	 (lexical-mlet 
	  monad-parse
	  ((s (=satisfies (par #'in literals #'eq))))
	  (m-return `(:literal ,s)))
	 (lexical-mlet 
	  monad-parse 
	  ((s (=satisfies #'symbolp)))
	  (m-return `(:symbol ,s))))))

(lex-defun =symbol-lit-or-sub (literals)
  (=or (=symbol-or-lit literals)
	   (=sub-expression literals)))

(defun =symbol ()
  (=satisfies #'symbolp))
(defun =list ()
  (=satisfies #'listp))
(lex-defun =sub-expression (literals)
  (lexical-mlet 
   monad-parse
   ((form (=list)))
   (m-return 
	`(:sub-expression
	  ,(parse-sequence (=pattern literals) form)))))

(defun =elipses ()
  (=satisfies (par #'eq '...)))

(lex-defun =and-more-terminator (literals)
  (lexical-mlet monad-parse
				((expr (=symbol-lit-or-sub literals))
				 (elipses (=elipses)))
				(m-return `(:one-or-more ,expr))))


(lex-defun =non-terminator (literals)
  (=and (=not (=and-more-terminator literals))
		(=symbol-lit-or-sub literals)))

(cl-prettyprint (parse-sequence (zero-or-more (=non-terminator '(x y))) '(x a v (y b ...) z ...)))


(lex-defun =pattern (literals)
  (lexical-mlet monad-parse
				((parts (zero-or-more (=non-terminator literals)))
				 (term (zero-or-one (=and-more-terminator literals))))
				(if term 
					(m-return (suffix parts term))
				  (m-return parts))))


(cl-prettyprint (parse-sequence (=pattern '(x q)) '(a b c (y q ...) x ...)))
((:symbol a)
 (:symbol b)
 (:symbol c)
 (:sub-expression ((:symbol y) (:one-or-more (:literal q))))
 (:one-or-more (:literal x)))


(defun =symbol-or-list ()
  (=or (=symbol) (=list)))
(defun =non-tail-element (literals)
  (=and (=not (=pattern-tail))
		(=symbol-or-list)))

(defun =pattern-tail ()
  (lexical-mlet 
   monad-parse
   ((head-pattern (=symbol-or-list))
	(elipses (=elipses)))
   (m-return (list :one-or-more head-pattern))))

(defun =pattern ()
  (lexical-mlet 
   monad-parse
   ((body (zero-or-more (=non-tail-element)))
	(tail (zero-or-one (=pattern-tail))))
   (m-return (append body tail))))

(parse-sequence #'=pattern (list 'a))

(parse-sequence (=pattern) '((a b c) c ...))

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

  