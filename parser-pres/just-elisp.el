(require 'el-pres)
(loop for page in (sort-by-page-ascending (safe-get-pages)) do
	  (insert-file page)
	  (goto-char (point-max)))
;;; Monadic Parser Combinators
;;; A Ground up Introduction

;; The best way, I think, to understand how these things works is to
;; consider the question of what a monadic parser combinator is in
;; the following order:

;;     1) What is our representation of a parse?
;;     2) How do we combine them?
;;     3) How does this combination strategy form a monad?

;; Depending on your temperament, you might not even care about 3,
;; which is fine.  The parser monad is useful without worrying too
;; hard about how monads work in general, but we will try to make
;; that clear in the course of the presentation.

(require 'el-pres)
   
;;;Controls Home    . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index
;;; Parsers

;;; The whole point here is to enable us to build complex parsers out
;;; of simple ones.  
;;; 
;;; A simple parser is a function which takes an input and returns either:
;;;  * nil, if the parser doesn't see what it wants
;;;  * or a pair ( produced-value . left-over-input )

;;; eg:

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
   
;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Atomic Parsers!

;;; You could imagine a ton of "parse-_" style parsers, but turns out
;;; there are even simpler parsers:

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


;;; This takes a value and returns a parser which "returns" that
;;; value, without changing the input.  If you wanted to insert a
;;; value into your parsers for some reason, this is the function
;;; you'd use.  
;;;
;;; It, too, will be of importance later.

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Combining Parsers

;;; Our goal is to make writing parsers as easy as writing programs.
;;; We program by combining simple functions.  How do we combine
;;; simple parsers?

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
(parse-ab "atrophy")
(parse-ab "oboe")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Meditation Upon Combination

;;; parse-ab was a mess.  Can we factor out this complexity?

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

(funcall (combine-parsers #'parse-a #'parse-b) "abraham a)")

;;; pretty sweet!

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Limitations of combine-parsers 

;;; Combine-parsers works ok when we want to combine two parsers.  We
;;; can even use the with argument to shoehorn more parsers together.

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

;;; But that is really pretty inconvenient.  And if we want to combine
;;; parsers which depend on the results of previous parsings,
;;; "combine-parsers" won't cut it.

;;; The crux of the issue is that we are really interested in the
;;; VALUE our parsers return, when combining parsers.  We need an
;;; interface to expose these values selectively.  

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Parser Bind

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

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; A bit more about bind.

(find-file-other-frame "~/work/art/monadic-types-of-interest.png")
(find-file-other-frame "~/work/art/bind.png")

;;; * Bind is kind of unintuitive.
;;; * However, it is more useful than "combine" because 
;;;   it facilitates sequencing.
;;; * bind's second argument is a lambda
;;; * a lambda is a delayed computation which depends on 
;;;   _unbound_ values.
;;; * bind _binds_ these values in an ordered way, facilitating
;;;   the sequencing of computations which result in monadic 
;;;   values.

;;; In the parser monad:
;;; * each lambda is a "delayed computation" which results in a 
;;;   _new parser_ when it is called with the value produced
;;;   by a previous parser.
;;; * bind combines the new parser with the old parser, 
;;;   handling the plumbing needed to connect them together.
;;; * this plumbing is
;;;    - check for nil
;;;    - wrap up everything in a containing parser.


;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Oh Dang it is the Lisp Slide

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

(funcall (parser-let* ((a-res #'parse-a)
			 (b-res #'parse-b)
			 (c-res #'parse-c))
					 (simple-parser-return (list a-res b-res c-res)))
		 "abcdef")

;;; ZING!

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Demystifying the Macro Magic

;;; consider that :

(let* ((x 10)
	   (y 11))
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
							   
							   
;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Non-trivial Things

;;; Ok, what kinds of fun things can we do with this parser monad
;;; business?

;;; Well, imagine you wish to match either:
;;; ab
;;; bc or
;;; ca

;;; We can do this with a single expression using our monadic parser
;;; combinators.  Observe:


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

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Useful Combinators

(defun -satisfies (pred)
  (lexical-let ((pred pred))
	(parser-let* 
	 ((item #'anything))
	 (if (funcall pred item) 
		 (simple-parser-return item)
	   #'nil-parser))))

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
	  (unless (empty? input)
		(recur-let 
		 ((rem-parsers parsers))
		 (cond
		  ((empty? rem-parsers) nil)
		  (t 
		   (let ((r (funcall (car rem-parsers) input)))
			 (if r r
			   (recur (cdr rem-parsers)))))))))))

;;; example:

(defun -cat-or-dog ()
  (parser-let* ((res (-or (-matches "cat")
						  (-matches "dog"))))
			   (simple-parser-return res)))

(funcall (-cat-or-dog) "ewe")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; More Combinators 

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
		 "dog dog dog dog cat")

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

(defun pempty? (input)
  "Check to see if you have hit the end of the input."
  (if (empty? input) (pair t input)
	(pair nil input)))

(defun -list (parser)
  (lexical-let ((parser parser))
	(parser-let* ((r parser))
				 (simple-parser-return 
				  (list r)))))

(defun -not (parser)
  (lexical-let ((parser parser))
	(lambda (input)
	  (unless (empty? input)
		(let ((r (funcall parser input)))
		  (if r nil
			(pair t input)))))))

(defun -and2 (p1 p2)
  (lexical-let ((p1 p1)
				(p2 p2))
	(parser-let* ((v1 p1)
				  (v2 p2))
				 (simple-parser-return v2))))

(defun -and (&rest ps)
  (reduce #'-and2 ps))

(defun -and-list (&rest ps)
  (lexical-let ((ps ps))
	(if (empty? ps)
		(lambda (input)
		  (pair nil input))
	  (parser-let* 
	   ((v (car ps))
		(rest (apply #'-and-list (cdr ps))))
	   (simple-parser-return (cons v rest))))))

(defun -n-of (n parser)
  (if (= n 1) (-list parser)
	(lexical-let ((n n)
				  (parser parser))
	  (parser-let* 
	   ((head parser)
		(rest (-n-of (- n 1) parser)))
	   (simple-parser-return (cons head rest))))))

(funcall (-n-of 3 (-matches "a")) "aaab")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index;;; Example

;;; From RFC 1459, the IRC Chat Protocol Standards Docoument a Pseudo
;;; BNF description of an IRC Message.  Lets write a parser for this.

;;; IRC MESSAGE:
;; <message> ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <prefix> ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
;; <command> ::= <letter> { <letter> } | <number> <number> <number>
;; <SPACE> ::= ' ' { ' ' }
;; <params> ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
;; <middle> ::=
;;   <Any *non-empty* sequence of octets not 
;;   including SPACE or NUL or CR or LF, the 
;;   first of which may not be ':'>
;; <trailing> ::=
;;   <Any, possibly *empty*, sequence of octets 
;;   not including NUL or CR or LF>
;; <crlf> ::= CR LF



;;; We'll just assume that the line feed has been removed by a
;;; pre-parser that feeds us lines.

(defun -trailing ()
  (parser-let* ((trailing (-zero-or-more #'anything)))
			   (simple-parser-return
				(list :trailing (reduce #'concat trailing)))))

(defun -colon-then-trailing ()
  (parser-let* ((colon (-colon))
				(trailing (-trailing)))
			   (simple-parser-return trailing)))

(defun -colon ()
  (-matches ":"))

(setq tab (format "\t"))
(defun -whitespaces ()
  (-one-or-more (-or (-matches " ")
					 (-matches tab))))

(defun -middle ()
  (parser-let* 
   ((colon (-not (-colon)))
	(contents (-zero-or-more (-not-whitespace))))
   (simple-parser-return (list :middle (reduce #'concat contents)))))

(defun -space-middle ()
  (parser-let* 
   ((_ (-whitespaces))
	(middle (-middle)))
   (simple-parser-return middle)))

(setq tab (format "\t"))
(defun -whitespaces ()
  (-one-or-more (-or (-matches " ")
					 (-matches tab))))

(defun -params ()
  (parser-let*
   ((params (-zero-or-more (-space-middle)))
	(_ (-whitespaces))
	(trailing (-maybe (-colon-then-trailing))))
   (simple-parser-return
	(cons (list :params 
				(mapcar #'cadr params))
		  (if trailing (list trailing)
			nil)))))

(defun -not-whitespace ()
  (-satisfies
   (lambda (x)
	 (and (not (string= x " "))
		  (not (string= x tab))))))

(defun -not-whitespaces ()
  (-zero-or-more (-not-whitespace)))

(lexical-let ((letters 
			   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
			  (numbers "1234567890")
			  (punctuation 
			   "~`!@#$%^&*()_+-={}[]|\\/<>,.:;'\"?"))
  (defun -letter ()
	(-satisfies
	 (lambda (x) 
	   (in (regexp-quote x) letters))))
  (defun -number ()
	(-satisfies 
	 (lambda (x)
	   (in (regexp-quote x) numbers))))
  (defun -punctuation ()
	(-satisfies 
	 (lambda (x)
	   (in (regexp-quote x) punctuation)))))

(defun -command ()
  (parser-let* 
   ((command (-or 
			  (-one-or-more (-letter))
			  (-n-of 3 (-number)))))
   (simple-parser-return
	(list :command (reduce #'concat command)))))

;;; We are going to cheat for the sake of brevity, and define prefix as:

(defun -prefix ()
  (parser-let* ((contents (-zero-or-more (-not-whitespace))))
			   (simple-parser-return (list :prefix (reduce #'concat contents)))))

;;; Putting it all together:

(defun -irc-message ()
  (parser-let* 
   ((_ (-colon))
	(prefix (-prefix))
	(_ (-whitespaces))
	(command (-command))
	(params&tail (-params)))
   (simple-parser-return
	(append (list prefix command) params&tail))))


(parsed-value (funcall (-irc-message) ":tod.com SEND a b c :rest"))

;;; WEEEEE




;;;Controls Home   