(require 'monads)
(require 'utils)
(require 'eieio)
(require 'cl)

(defn parser-bind [parser fun]
  (fn [input]
	  (loop for (value . input) in (funcall parser input) 
			append (funcall (funcall fun value) input))))

(defn parser-return [val]
  (fn [input]
	  (list (cons val input))))

(setq monad-parse 
	  (tbl! 
	   :m-return #'parser-return
	   :m-bind   #'parser-bind))

(defclass <parser-input-string> () 
  ((data :accessor string-of :initarg :string)
   (ix   :accessor index-of  :initarg :index :initform 0)))

(defmethod input-empty? ((input <parser-input-string>))
  (= (length (string-of input)) (index-of input)))
(defmethod input-empty-p ((input <parser-input-string>))
  (= (length (string-of input)) (index-of input)))

(defmethod input-first ((input <parser-input-string>))
  (elt (string-of input) (index-of input)))

(defmethod input-rest ((input <parser-input-string>))
  (make-instance '<parser-input-string> :string 
				 (string-of input)
				 :index (+ 1 (index-of input))))

(defun empty-string-parser ()
  (make-instance '<parser-input-string>
				 :string "" :index 0))

(defmethod input-as-string ((input <parser-input-string>))
  (substring (string-of input) (index-of input) (length (string-of input))))

(defun string->parser-input (str)
  (make-instance '<parser-input-string>
				 :string str))

(defun parser-fail ()
  (lambda (input) nil))

(defun parser-item ()
  (lambda (input)
	(unless (input-empty? input)
	  (list (cons (input-first input)
				  (input-rest input))))))

(funcall (parser-item) (string->parser-input ""))

(defun =satisfies (predicate)
  (lexical-let ((lpred predicate))
	(parser-bind (parser-item)
				 (lambda (x) 
				   (if (funcall lpred x)
					   (parser-return x)
					 (parser-fail))))))

(lexical-let ((digits (coerce "1234567890" 'list)))
  (defun digit-char? (x)
	(in x digits)))

(lexical-let ((lowers (coerce "abcdefghijklmnopqrztuvwxyz" 'list))
			  (uppers (coerce "ABCDEFGHIJKLMNOPQRZTUVWXYZ" 'list)))
  (defun upper-case-char? (x)
	(in x uppers))
  (defun lower-case-char? (x)
	(in x lowers)))

(defun =char (x)
  (lexical-let ((x x))
	(=satisfies (lambda (y) (eql x y)))))
(defun =upper-case-char? ()
  (=satisfies (lambda (y) (upper-case-char? y))))
(defun =lower-case-char? ()
  (=satisfies (lambda (y) (lower-case-char? y))))

(defun =digit-char ()
  (=satisfies #'digit-char?))

(defun parser-plus (p1 p2)
  (lexical-let ((p1 p1)
				(p2 p2))
	(lambda (input) 
	  (append (funcall p1 input) (funcall p2 input)))))

(defun letter () (parser-plus (=lower-case-char?) (=upper-case-char?)))

(defun alphanumeric () (parser-plus (=digit-char) (letter)))

(defun =string (input)
  (lexical-let ((input input))
	(if (input-empty? input)
		(parser-return (empty-string-parser))
	  (domonad monad-parse 
			   [_ (=char (input-first input))
				  _ (=string (input-rest input))]	(print input)
				  input))))

(lex-defun =or (parser &rest parsers)
		   (lambda (input)
			 (or (funcall parser input)
				 (when parsers
				   (funcall (apply #'=or parsers) input)))))

(lex-defun =not (parser)
		   (lambda (input)
			 (let ((result (funcall parser input)))
			   (if result
				   nil
				 (list (cons t input))))))

(defmacro* =let* (forms &body body)
  `(domonad monad-parse ,forms ,@body))

(lex-defun =and (p1 &rest ps)
		   (=let* [result p1]
				  (if ps
					  (apply #'=and ps)
					result)))

(defun parser-maybe (parser)
  (=or parser (parser-return nil)))

(defun letters ()
  (=or (=let* [x (letter)
				 xs (letters)]
			  (cons x xs))
	   (parser-return nil)))

(lex-defun zero-or-more (parser)
		   (=or (=let* [x parser
						  xs (zero-or-more parser)]
					   (cons x xs))
				(parser-return nil)))


(lex-defun one-or-more (parser)
		   (=let* [x parser
					 y (zero-or-more parser)]
				  (cons x y)))

(provide 'monad-parse)
