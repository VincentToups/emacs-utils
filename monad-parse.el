(require 'monads)
(require 'utils)
(require 'eieio)
(require 'cl)
(require 'defn)
(require 'functional)

;; (defn parser-bind [parser fun]
;;   (fn [input]
;; 	  (loop for (value . input) in (funcall parser input) 
;; 			append (funcall (funcall fun value) input))))

(defun parser-bind (parser fun)
  (lexical-let ((parser parser)
				(fun fun))
	(lambda (input)
	  (lexical-let ((input input))
		(loop for (value . input) in (funcall parser input) 
			  append (funcall (funcall fun value) input))))))

(defn parser-return [val]
  (fn [input]
	  (list (cons val input))))

;; (defun parser-return (val)
;;   (lexical-let ((val val))
;; 	(lambda (input) 
;; 	  (list (cons val input)))))

(defun parser-zero (&optional val)
  (lambda (input) 
	(lexical-let ((input input))
	  (list (cons nil input)))))

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

(defclass <parser-input-sequence> ()
  ((data :accessor seq-of :initarg :seq)
   (ix   :accessor index-of :initarg :index :initform 0)))

(defmethod input-empty? ((input <parser-input-sequence>))
  (= (length (seq-of input)) (index-of input)))
(defmethod input-empty-p ((input <parser-input-sequence>))
  (= (length (seq-of input)) (index-of input)))
(defmethod input-first ((input <parser-input-sequence>))
  (elt (seq-of input) (index-of input)))
(defmethod input-rest ((input <parser-input-sequence>))
  (make-instance '<parser-input-sequence> :seq 
				 (seq-of input)
				 :index (+ 1 (index-of input))))



(defclass <parser-input-buffer> () 
  ((buffer :accessor buffer-of :initarg :buffer)
   (ix   :accessor index-of  :initarg :index :initform 1)))

(defmethod input-empty-p ((input <parser-input-buffer>))
  (with-current-buffer (buffer-of input)
	(if (= (index-of input) (point-max)) t nil)))

(defmethod input-empty? ((input <parser-input-buffer>))
  (with-current-buffer (buffer-of input)
	(if (= (index-of input) (point-max)) t nil)))

(defmethod input-first ((input <parser-input-buffer>))
  (with-current-buffer 
	  (buffer-of input)
	(let ((ix (index-of input)))
	  (elt (buffer-substring ix (+ 1 ix)) 0))))

(defmethod input-rest ((input <parser-input-buffer>))
  (make-instance '<parser-input-buffer>
				 :buffer (buffer-of input)
				 :index (+ (index-of input) 1)))

(defmethod input-as-string ((input <parser-input-buffer>))
  (with-current-buffer/save-excursion 
   (buffer-of input)
   (buffer-substring (index-of input) (- (point-max) 1))))

(defun input->string (input)
  (if input (input-as-string input) nil))  

(defmethod input-as-list ((input <parser-input-sequence>))
  (elts (seq-of input)
		(range (index-of input)
			   (length (seq-of input)))))

(defun buffer->parser-input (buffer-or-name)
  (make-instance '<parser-input-buffer>
				 :buffer (get-buffer buffer-or-name)
				 :index 1))


(defun empty-string-parser ()
  (make-instance '<parser-input-string>
				 :string "" :index 0))

(defmethod input-as-string ((input <parser-input-string>))
  (substring (string-of input) (index-of input) (length (string-of input))))

(defun string->parser-input (str)
  (make-instance '<parser-input-string>
				 :string str))

(defun sequence->parser-input (seq)
  (make-instance '<parser-input-sequence>
				 :seq (coerce seq 'vector)))

(defun parser-fail ()
  (lambda (input) nil))

(defun parser-item ()
  (lambda (input)
	(unless (input-empty? input)
	  (list (cons (input-first input)
				  (input-rest input))))))

(lex-defun parser-items (n)
  (lambda (input)
	(let ((i 0)
		  (ac nil))
	  (loop while (and (< i n)
					   (not (input-empty? input)))
			do
			(setq i (+ i 1))
			(push (input-first input) ac )
			(setq input (input-rest input)))
	  (if (= (length ac) n) (list (cons (reverse ac) input) nil)))))

(lex-defun parser-items->string (n)
  (lambda (input)
	(let ((i 0)
		  (ac nil))
	  (loop while (and (< i n)
					   (not (input-empty? input)))
			do
			(setq i (+ i 1))
			(push (input-first input) ac )
			(setq input (input-rest input)))
										;(db-print (list n (length ac) (coerce (reverse ac) 'string)))
	  (if (= (length ac) n) (list (cons (coerce (reverse ac) 'string) input) nil)))))

(defun =string (str)
  (lexical-let ((str str))
	(parser-bind (parser-items->string (length str))
				 (lambda (x)
				   (if (string= x str)
					   (parser-return x)
					 (parser-fail))))))
(defun =string->seq (str)
  (lexical-let ((str str))
	(parser-bind (parser-items->string (length str))
				 (lambda (x)
				   (if (string= x str)
					   (parser-return (coerce x 'list))
					 (parser-fail))))))

(funcall (parser-item) (string->parser-input ""))

(defun =satisfies (predicate)
  (lexical-let ((lpred predicate))
	(parser-bind (parser-item)
				 (lambda (x) 
				   (if (funcall lpred x)
					   (parser-return x)
					 (parser-fail))))))

(defun* =is (object &optional (pred #'eq))
  (lexical-let ((object object)
				(pred pred))
	(=satisfies 
	 (lambda (x) (funcall pred object x)))))

(lexical-let ((digits (coerce "1234567890" 'list)))
  (defun digit-char? (x)
	(in x digits)))

(defun ->in (x)
  (cond 
   ((and (stringp x) (bufferp (get-buffer x)))
	(buffer->parser-input x))
   ((stringp x)
	(string->parser-input x))
   ((sequencep x)
	(sequence->parser-input x))
   (t (error "Can't convert %s into a parser input." x))))

(lexical-let ((lowers (coerce "abcdefghijklmnopqrsztuvwxyz" 'list))
			  (uppers (coerce "ABCDEFGHIJKLMNOPQRSZTUVWXYZ" 'list)))
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

;; (defun =digit-char->string ()
;;   (=let* [c (=digit-char)]
;; 		 (if c (coerce (list c) 'string) nil)))
(defun =digit-char->string ()
  (=simple-let* ((c (=digit-char)))
				(if c (coerce (list c) 'string) nil)))


(defun =digit-char->number ()
  (=let* [c (=digit-char)]
		 (if c (read (coerce (list c) 'string)) nil)))


(defun parser-plus-2 (p1 p2)
  (lexical-let ((p1 p1)
				(p2 p2))
	(lambda (input) 
	  (append (funcall p1 input) (funcall p2 input)))))

(defun parser-plus (&rest args)
  (reduce #'parser-plus-2 args))

(defun letter () (parser-plus (=lower-case-char?) (=upper-case-char?)))

(defun alphanumeric () (parser-plus (=digit-char) (letter)))

(defun =char->string (char)
  (=let* [_ (=char char)]
		 (coerce (list _) 'string)))

(defun =point ()
  (=satisfies (par #'eq ?.)))

(defun =decimal-part ()
  (=simple-let*
   ((dot (=point))
	(rest (=one-or-more (=digit-char))))
   (coerce (cons dot rest) 'string)))

(defun =integer-part ()
  (=simple-let* 
   ((digits (=zero-or-more (=digit-char))))
   (coerce digits 'string)))

(defun =number->number ()
  (=simple-let* 
   ((int (=integer-part))
	(dec (=maybe (=decimal-part))))
   (string-to-number (concat int dec))))

(lex-defun =or2 (p1 p2)
  (lambda (input)
	(or (funcall p1 input)
		(funcall p2 input))))
(lex-defun =or (&rest ps)
  (reduce #'=or2 ps))

;; (lex-defun =or (parser &rest parsers)
;; 		   (lambda (input)
;; 			 (or (funcall parser input)
;; 				 (when parsers
;; 				   (funcall (apply #'=or parsers) input)))))

;; (lex-defun =or (parser &rest parsers)
;; 		   (lambda (input)
;; 			 (foldl 
;; 			  (lambda (sub-parser state)
;; 				(or state
;; 					(funcall sub-parser input)))
;; 			  (funcall parser input)
;; 			  parsers)))

(lex-defun =not (parser)
  b
  (lambda (input)
	(let ((result (funcall parser input)))
	  (if result
		  nil
		(list (cons t input))))))

(defmacro* =let* (forms &body body)
  `(lexical-domonad< monad-parse ,forms ,@body))

(defmacro* =simple-let* (bindings &body body)
  (if bindings 
	  (let ((symbol (car (car bindings))))
		`(parser-bind ,@(cdr (car bindings))
					  (lex-lambda (,symbol) 
								  (=simple-let* ,(cdr bindings)
												,@body))))
	`(parser-return (progn ,@body))))

(lex-defun =and2 (p1 p2)
  (lex-lambda (input)
			  (and (funcall p1 input)
				   (funcall p2 input))))

(lex-defun =and (&rest ps)
  (reduce #'=and2 ps))

;; (lex-defun =and (p1 &rest ps)
;; 		   (=let* [result p1]
;; 				  (if ps
;; 					  (apply #'=and ps)
;; 					result)))

(lex-defun =and-concat2 (p1 p2)
  (=let* [r1 p1
			 r2 p2]
		 (concat r1 r2)))

(lex-defun =and-concat (&rest ps)
  (reduce #'=and-concat2 ps))

(lex-defun parser-maybe (parser)
  (=or parser (parser-return nil)))
(lex-defun =maybe (parser)
  (=or parser (parser-return nil)))


(defun letters ()
  (=or (=let* [x (letter)
				 xs (letters)]
			  (cons x xs))
	   (parser-return nil)))

(defun =letter ()
  (letter))

;; (lex-defun zero-or-more (parser)
;; 		   (=or (=let* [x parser
;; 						  xs (zero-or-more parser)]
;; 					   (cons x xs))
;; 				(parser-return nil)))

(lex-defun =zero-or-one (parser)
  (=or (=let* [_ parser]
			  _)
	   (parser-return nil)))

(lex-defun zero-or-one-list (parser)
  (=or (=let* [_ parser]
			  (list _))
	   (parser-return nil)))

(lex-defun zero-or-plus-more (parser)
  (lambda (input)
	(let ((terminals nil)
		  (continuers (funcall (zero-or-one-list parser) input))
		  (done nil)
		  (res nil))
	  (loop while (not done) do
			(let ((old-continuers continuers))
			  (setq continuers nil)
			  (loop while old-continuers
					do
					(let* ((sub-parser-state (pop old-continuers))
						   (state (car sub-parser-state))
						   (sub-input (cdr sub-parser-state))
						   (res (funcall parser sub-input)))
					  (if res
						  (setq continuers
								(append continuers (mapcar 
													(lambda (sub-res)
													  (cons
													   (suffix state (car sub-res))
													   (cdr sub-res)))
													res)))
						(push sub-parser-state terminals)))))
			(if (empty? continuers)
				(setq done t)))
	  terminals)))

(lex-defun zero-or-more 
  (parser)
  (lexical-let ((zero-or-one-parser (zero-or-one parser)))
	(lex-lambda (input)
				(let* ((sub-state (car (funcall (zero-or-one-list parser) input)))
					   (acc (car sub-state))
					   (done (not (car sub-state))))

				  (if done (list sub-state)
					(progn 

					  (loop while (not done) do
							(let* ((next-input (cdr sub-state))
								   (next-sub-state 
									(car (funcall zero-or-one-parser next-input)))
								   (res (car next-sub-state)))
							  (if res (progn
										(push res acc)
										(setq sub-state next-sub-state))
								(setq done t))))
					  (list (cons (reverse acc) (cdr sub-state)))))))))

(lex-defun =zero-or-more 
  (parser)
  (lexical-let ((zero-or-one-parser (zero-or-one parser)))
	(lex-lambda (input)
				(let* ((sub-state (car (funcall (zero-or-one-list parser) input)))
					   (acc (car sub-state))
					   (done (not (car sub-state))))

				  (if done (list sub-state)
					(progn 

					  (loop while (not done) do
							(let* ((next-input (cdr sub-state))
								   (next-sub-state 
									(car (funcall zero-or-one-parser next-input)))
								   (res (car next-sub-state)))
							  (if res (progn
										(push res acc)
										(setq sub-state next-sub-state))
								(setq done t))))
					  (list (cons (reverse acc) (cdr sub-state)))))))))



(lex-defun one-or-more 
  (parser)
  (=let* [x parser
			y (zero-or-more parser)]
		 (cons x y)))

(lex-defun =one-or-more 
  (parser)
  (=let* [x parser
			y (zero-or-more parser)]
		 (cons x y)))


(defun parse-string (parser string)
  (car (car (funcall parser (string->parser-input string)))))

(defun parse-sequence (parser sequence)
  (car (car (funcall parser (->in sequence)))))

(defun parse-string-det (parser string)
  (let* ((pr (funcall parser (->in string)))
		 (result (car (car pr)))
		 (rest (input->string (cdr (car pr)))))
	(if (or (not result)
			(not rest)) nil
	  (list result rest))))

(defun =lit-sym (sym)
  (=satisfies (f-and 
			   #'symbolp 
			   (par #'eq sym))))

(provide 'monad-parse)

