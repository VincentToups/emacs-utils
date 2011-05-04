(defun str-head (str)
  (substring str 0 1))
(defun str-tail (str)
  (substring str 1))
(defun pair (a b)
  (cons a b))
(defun parsed-value (pair)
  (car pair))
(defun parsed-leftover (pair)
  (cdr pair))


(defun parse-a (input)
  "A very simple parser - parses 'a' or nothing."
  (unless (empty? input)
	(if (string= "a" (str-head input))
		(pair :found-a (str-tail input))
	  nil)))

(parse-a "abracadabra")
(parse-a "dogs of war")

(defun anything (input) ; aka "item"
  (unless (empty? input)
	(pair (str-head input) (str-tail input))))

;;; And one very important _parameterized_ parser:

(defun simple-parser-return (val)
  (lexical-let ((val val))
	(lambda (input)
	  (pair val input))))

(defun nil-parser (input)
  nil)

(defun parse-b (input)
  (unless (empty? input)
	(if (string= (str-head input) "b")
		(pair :found-b (str-tail input)))))

(defun parse-ab (input)
  (unless (empty? input)
	(let ((a-result (parse-a input)))
	  (if a-result 
		  (let* ((a-val (parsed-value a-result))
				 (new-input (parsed-leftover a-result))
				 (b-result (parse-b new-input)))
			(if b-result
				(let* ((b-val (parsed-value b-result)))
				  (pair (list a-val b-val) 
						(parsed-leftover b-result)))))))))

(parse-ab "abracadabra")
(parse-ab "atropy")
(parse-ab "oboe")

(defun* combine-parsers (p1 p2 &optional (with #'list))
  (lexical-let ((p1 p1)
				(p2 p2)
				(with with)) ; create lexical copies of p1 and p2
										; since we are returning a lambda that
										; which depends on them.
	(lambda (input) 
	  (unless (empty? input)
		(let ((r1 (funcall p1 input)))
		  (if r1 
			  (let* ((v1 (parsed-value r1))
					 (leftover1 (parsed-leftover r1))
					 (r2 (funcall p2 leftover1)))
				(if r2 
					(pair (funcall with v1
								   (parsed-value r2))
						  (parsed-leftover r2))))))))))

;;; COMBINE-PARSERS is a *combinator* or higher order function in the
;;; functional-programming sense.  It is a function which operates on
;;; functions and returns a new function.

(funcall (combine-parsers #'parse-a #'parse-b) "abraham a")

(defun parse-c (input)
  (unless (empty? input)
	(if (string= (str-head input) "c")
		(pair :found-c (str-tail input))
	  nil)))

(defun parse-a-b-c (input)
  (funcall
   (combine-parsers 
	(combine-parsers #'parse-a #'parse-b) 
	#'parse-c #'suffix)
   input))

(parse-a-b-c "abcdef")

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

(funcall (parser-let* ((a-res #'parse-a)
			 (b-res #'parse-b)
			 (c-res #'parse-c))
					 (simple-parser-return (list a-res b-res c-res)))
		 "abcdef")


(defun parse-a|b|c (input)
  (unless (empty? input)
	(string-case (str-head input)
				 ("a" (pair :found-a (str-tail input)))
				 ("b" (pair :found-b (str-tail input)))
				 ("c" (pair :found-c (str-tail input))))))

(defun make-dependent-parser (last-result)
  (case last-result
	(:found-a #'parse-b)
	(:found-b #'parse-c)
	(:found-c #'parse-a)))

(setq triangle-parser 
	   (parser-let* ((first-char #'parse-a|b|c)
					 (second-char (make-dependent-parser first-char)))
					(simple-parser-return (cons first-char second-char))))

(funcall triangle-parser "ab")
(funcall triangle-parser "bc")
(funcall triangle-parser "ca")
(funcall triangle-parser "aa")
(funcall triangle-parser "cq")

(find-file-other-frame "~/work/art/haskell-curry-says.png")

;;; Useful Combinators

(defun -satisfies (pred)
  (lexical-let ((pred pred))
	(parser-let* 
	 ((item #'anything))
	 (if (funcall pred item) 
		 (simple-parser-return item)
	   nil))))

(defun -manythings (n)
  (lexical-let ((n n))
	(lambda (input)
	  (if (< (length input) n) nil
		(pair 
		 (substring input 0 n)
		 (substring input (min (length input) n)))))))

(defun -matches (str)
  (lexical-let ((str str)) ; parser-let* implicitely 
                          ; constructs a function
                          ; which requires str
			  (parser-let*
			   ((sub (-manythings (length str))))
			   (if (string= sub str)
				   (simple-parser-return sub)
				 #'nil-parser))))

;;; because of the behavior of bind, we can't write the following
;;; function with parser-let*:

(require 'recur)
(defun -or (&rest parsers)
  (lexical-let ((parsers parsers))
	(lambda (input)
	  (recur-let 
	   ((rem-parsers parsers))
	   (cond
		((empty? rem-parsers) nil)
		(t 
		 (let ((r (funcall (car rem-parsers) input)))
		   (if r r
			 (recur (cdr rem-parsers))))))))))

;;; example:

(defun -cat-or-dog ()
  (parser-let* ((res (-or (-matches "cat")
						  (-matches "dog"))))
			   (simple-parser-return res)))

(funcall (-cat-or-dog) "ewe")

(defun -zero-or-more (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(recur-let ((result (funcall parser input))
					(acc nil)
					(last-input input))
				   (if result
					   (recur
						(funcall parser (parsed-leftover result))
						(cons (parsed-value result) acc)
						(parsed-leftover result))
					 (pair (reverse acc)
						   last-input)))))))

(funcall (-zero-or-more 
		  (-matches "a"))
		 "aaaab")

(defun -one-or-more (parser)
  (lexical-let ((parser parser))
	(parser-let* ((first parser)
				  (rest (-zero-or-more parser)))
				 (simple-parser-return (cons first rest)))))

(funcall (-one-or-more
		  (-matches "dog "))
		 "cat dog dog dog cat")

(funcall (-zero-or-more 
		  (-matches "dog "))
		 "cat dog dog dog cat")

(defun -maybe (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(let ((r (funcall parser input)))
		  (if r r
			(pair nil input)))))))
