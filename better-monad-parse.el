(require 'monads)
(require 'utils)
(require 'cl)
(require 'defn)
(require 'functional)
(require 'multimethods)

;;; Uniform Input Represention:

(defstruct buffer-input (buffer :read-only) (index :read-only))
(defun create-buffer-input (buf)
  "Create an initialized buffer-input struct for the buffer BUF."
  (make-buffer-input :buffer (get-buffer buf) :index 1))

(defun set-buffer-input-index (b i)
  "Functionally modify the buffer-input B's index to I.  Original structure
remains unmodified.  If B is a regular buffer, it is promoted to a 
buffer-input."
  (cond 
   ((bufferp b) (make-buffer-input :buffer (get-buffer b) :index i))
   ((buffer-input-p b)
	(make-buffer-input 
	 :buffer
	 (buffer-input-buffer b) 
	 :index i))))

(defun incr-buffer-input-index (b)
  "Functionally increment a buffer input index."
  (cond
   ((bufferp b) (incr-buffer-input-index (create-buffer-input b)))
   ((buffer-input-p b)
	(let ((i (buffer-input-index b)))
	  (set-buffer-input-index b (+ i 1))))))

(defun input-dispatcher (thing)
  "Dispatch function for generic parser input methods."
  (cond 
   ((stringp thing) :string)
   ((listp thing) :list)
   ((buffer-input-p thing) :buffer-input)
   ((bufferp thing) :buffer)))

;;; Define predicate methods for detecting empty inputs.

(defmulti input-empty? #'input-dispatcher "Returns t if an input is empty.") 
(defunmethod input-empty? :string (s) (= (length s) 0))
(defunmethod input-empty? :list (l) (not l))

(defun point-max-of (buffer)
  "Retrieve (point-max) for a BUFFER."
  (with-current-buffer buffer
	  (point-max)))

(defunmethod input-empty? :buffer-input (b)
  (>= (buffer-input-index b) (point-max-of (buffer-input-buffer b))))

(defunmethod input-empty? :buffer (b)
  (if (= 1 (point-max-of b)) t nil))

(defalias 'input-emptyp #'input-empty?)

(defmulti input-first #'input-dispatcher "Get the first element of an input.")
(defunmethod input-first :string (s)
  (if (> (length s) 0) (substring s 0 1)
	(error "Can't take the first element of an empty string.")))
(defunmethod input-first :list (l)
  (if l (car l) (error "Can't take the fist element of an empty list.")))
(defunmethod input-first :buffer (b)
  (input-first (create-buffer-input b)))
(defunmethod input-first :buffer-input (bi)
  (with-current-buffer (buffer-input-buffer bi)
	(let ((i (buffer-input-index bi)))
	(buffer-substring i (+ i 1)))))

(defmulti input-rest #'input-dispatcher "Get an input representing subsequent elements of an input.")
(defunmethod input-rest :string (s)
  (substring s 1))
(defunmethod input-rest :list (l)
  (cdr l))
(defunmethod input-rest :buffer (b)
  (input-rest (create-buffer-input b)))
(defunmethod input-rest :buffer-input (bi)
  (incr-buffer-input-index bi))

;;; Monadic Functions:
;;; Parsers start with =
;;; Functions returning parsers start with =>

;;; It is convenient to treat parsers as both values and functions.  This
;;; form defines them as both.
(defmacro defun/var (name args &rest body)
  "Simultaneously define a function and set NAME's SYMBOL-VALUE to that function."
  `(progn
	 (defun ,name ,args ,@body)
	 (defvar ,name nil ,@(if (stringp (car body)) (list (car body)) nil))
	 (setq ,name #',name)))

(defun/var =nil (input) 
  "The nil parser.  The zero of the parser monad." 
  nil)
(defun/var =item (input) 
  "The one-item parser.  Parse the first item from the input, regardless of what it is."
  (unless (input-empty? input)
	(list (cons (input-first input)
				(input-rest input)))))

(defun/var =rest (input)
  "Get the parser state itself.  Useful for checking for the end of the input
within the monad."
  (list (cons input input)))

(defun =>items (n)
  "Produce the parser that gets the N first items from the parser
input or as many as possible when the input contains fewer
items."
  (enclose 
   (n)
   (lambda (input)
	 (recur-let 
	  ((acc '())
	   (input input)
	   (n n))
	  (cond
	   ((or (= n 0) 
			(input-empty? input))
		(list (cons (reverse acc) input)))
	   (t
		(recur (cons (input-first input) acc)
			   (input-rest input)
			   (- n 1))))))))

(defun parser-bind (parser =>parser)
  "Produce a new parser which represents the parser produced by =>PARSER on the
monadic return values of PARSER."
  (enclose 
   (parser =>parser)
   (lex-lambda 
	(input)
	(recur-let 
	 ((acc '())
	  (rs (funcall parser input)))
	 (if (empty? rs) acc
	   (let ((pair (car rs))
			 (rest (cdr rs)))
		 (recur (append (funcall 
						 (funcall =>parser (car pair))
						 (cdr pair)) acc)
				rest)))))))

(defun parser-return (thing)
  "Produce a parser which leaves its input unmodified and which
monadically returns THING."
  (enclose 
   (thing)
   (lambda (input)
	 (list (cons thing input)))))

(defun parser-plus (p1 p2)
  "Parser monadic plus operation - returns the parser which
parses P1 and then P2, with the monadic return value of P2."
  (parser-bind p1 (lambda (stub) p2)))

(defvar monad-parse 
  (tbl!
   :m-return #'parser-return
   :m-bind #'parser-bind
   :m-zero =nil
   :m-plus #'parser-plus)
  "The (better) parser monad.")

(defun parse/first-result (parser input)
  "Parse INPUT with PARSER and return the FIRST monadic return value."
  (car (car (funcall parser input))))

;;; Now we can use monadic binding to build some parsers.

(defun =>items->string (n)
  "Pull N items from the input (or fewer if fewer are in input) and 
return the results concatenated into a string."
  (enclose
   (n)
   (monadic-do 
	monad-parse
	(items-list <- (=>items n))
	(m-return (reduce #'concat items-list)))))

(defun =>satisfies (fun)
  "Parse one item IF it FUN is true for that item."
  (enclose 
   (fun)
   (monadic-do
	monad-parse
	(item <- =item)
	(if (funcall fun item)
		(m-return item)
	  =nil))))

(defun =>list (parser)
  "Wraps the monadic return value of PARSER into a LIST."
  (enclose 
   (parser)
   (monadic-do monad-parse
	(item <- parser)
	(m-return (list item)))))

;;; The dreaded zero plus more combinator.

(defun zero-plus-more-step (substate parser)
  "Apply PARSER to the CDR of substate.  If it succeeds, cons the
result onto the list in the CAR of substate and indicate CONTINUE
for MAPCAR/DEAL.  If PARSER on CDR of substate FAILS, then
reverse the CAR of SUBSTATE and return this value consed with the
last INPUT state."
  (let* ((mrv (car substate))
		 (input (cdr substate))
		 (r (funcall parser input)))
	(if r (list 
		   :continue 
		   (mapcar (lambda (subr)
					 (let ((r (car subr))
						   (rest (cdr subr)))
					   (cons (cons r mrv) rest)))
				   r))
	  (list :terminate 
			(cons (reverse mrv) input)))))

(defun =>zero-plus-more (p)
  "Produce a parser which parses P zero or more times and monadically
returns the results in a list."
  (enclose 
   (p)
   (lex-lambda 
	(input)
	(recur-let 
	 ((terminals nil)
	  (continuers (funcall (=>list p) input)))
	 (if (empty? continuers)
		 terminals 
	   (let* ((split-tbl
			   (mapcar/deal (par #'zero-plus-more-step p) continuers))
			  (new-continuers (alist split-tbl :continue))
			  (new-terminals (alist split-tbl :terminate)))
		 (recur (append terminals new-terminals)
				(reduce #'append new-continuers))))))))

(defun =>zero-or-more (p)
  "Alias for =>ZERO-PLUS-MORE."
  (=>zero-plus-more p))

(defun =>one-plus-more (p)
  "Parse P at least once and then accumlate as many P as possible."
  (enclose 
   (p)
   (monadic-do monad-parse
			   (first <- p)
			   (rest <- (=>zero-plus-more p))
			   (m-return (cons first rest)))))

(defun =>one-or-more (p)
  "Alias for =>ZERO-PLUS-MORE."
  (=>one-plus-more p))

(defun =>or2 (p1 p2)
  "Produce a PARSER which succeeds on P1 or P2.  Returns the
successful parser's value."
  (monadic-do 
   monad-parse
   (r <- p1)
   (if (not r) 
	   p2
	 (m-return r))))

(defun =>or (&rest ps)
  "Produce a parser which succeeds if one of PS is true."
  (reduce #'=>or ps))

(defun =>and2 (p1 p2)
  "Produce a parser which succeeds if P1 and P2 both succeed."
  (monadic-do 
   monad-parse
			   



(provide 'better-monad-parse)