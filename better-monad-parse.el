(require 'monads)
(require 'utils)
(require 'cl)
(require 'defn)
(require 'functional)
(require 'multi-methods)

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

(defstruct stateful-input (input :read-only) (state :read-only))

(defun input-dispatcher (thing)
  "Dispatch function for generic parser input methods."
  (cond 
   ((stringp thing) :string)
   ((listp thing) :list)
   ((buffer-input-p thing) :buffer-input)
   ((bufferp thing) :buffer)
   ((stateful-input-p thing) :stateful-input)))

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

(defunmethod input-empty? :stateful-input (s)
  (input-empty? (stateful-input-input s)))

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
(defunmethod input-first :stateful-input (s)
  (input-first (stateful-input-input s)))

(defmulti input-rest #'input-dispatcher "Get an input representing subsequent elements of an input.")
(defunmethod input-rest :string (s)
  (substring s 1))
(defunmethod input-rest :list (l)
  (cdr l))
(defunmethod input-rest :buffer (b)
  (input-rest (create-buffer-input b)))
(defunmethod input-rest :buffer-input (bi)
  (incr-buffer-input-index bi))
(defunmethod input-rest :stateful-input (s)
  (make-stateful-input :input
					   (input-rest (stateful-input-input s))
					   :state (stateful-input-state s)))

(defun push-dispatcher (item input)
  (input-dispatcher input))

(defmulti input-push #'push-dispatcher "Add an item to the front of an input")
(defunmethod input-push :string (item input) (concat (if (stringp item) item (format "%S" item)) input))
(defunmethod input-push :list (item input) (cons item input))
(defunmethod input-push :buffer (item input) (input-push item (create-buffer-input input)))
(defunmethod input-push :buffer-input (item input) 
  (warn "Pushing an input onto a buffer is non-functional.")
  (with-current-buffer (buffer-input-buffer input)
	(save-excursion 
	  (goto-char (buffer-input-index input))
	  (insert (if (stringp item) item (format "%S" item)))))
  input)


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

(defun parser-return (&rest things)
  "Produce a parser which leaves its input unmodified and which
monadically returns THING."
  (enclose 
   (things)
   (lambda (input)
	 (mapcar (par #'cons input) things))))

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

(defun =>maybe (parser)
  "Produces a parser that always succeeds.  If parser succeeds,
the monadic return value comes from PARSER.  Otherwise,
monadically returns NIL."
  (enclose 
   (parser)
   (lambda (input)
	 (let ((rs (funcall parser input)))
	   (if rs rs
		 (list (cons nil input)))))))

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
		 (if (empty? terminals) 
			 (list (cons nil input))
		   terminals) 
	   (let* ((split-tbl
			   (mapcar/deal (par #'zero-plus-more-step p) continuers))
			  (new-continuers (alist split-tbl :continue))
			  (new-terminals (alist split-tbl :terminate)))
		 (recur (append terminals new-terminals)
				(reduce #'append new-continuers))))))))

(defun =>zero-plus-more->string (p)
  "Like =>ZERO-PLUS-MORE, but the monadic return is concatenated
into a string."
  (=>reduce-concat (=>zero-plus-more p)))

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
  (enclose 
   (p1 p2)
   (lambda (input)
	 (let ((rs (funcall p1 input)))
	   (if rs rs
		 (funcall p2 input))))))
(defun =>or (&rest ps)
  "Produce a parser which succeeds if one of PS is true."
  (reduce #'=>or2 ps))

(defun =>and2 (p1 p2)
  "Produce a parser which succeeds if P1 and P2 both succeed."
  (monadic-do 
   monad-parse
   p1
   p2))

(defun =>and (&rest ps)
  "Produces a parser if all PS succeed.  Result is last parser's result."
  (reduce #'=>and2 ps))

(defun =>not (p)
  "Succeeds only if P fails.  Returns one item."
  (enclose 
   (p)
   (lambda (input)
	 (let ((rs (funcall p input)))
	   (if rs =nil
		 =item)))))

(defun =>reduce-concat (p)
  "Return a new parser that reduces the result of P with concat."
  (monadic-do
   monad-parse
   (r <- p)
   (m-return (reduce #'concat r))))

;;; We need to start writing 

(defmacro defvar/fun (name lambda &optional doc)
  `(progn 
	 (defvar ,name nil ,@(if doc (list doc) (list)))
	 (setq ,name ,lambda)
	 (defalias ',name ,name)))

(defun to-char (s)
  "Convert s to the character code representing its first
character."
  (car (coerce s 'list)))

(lexical-let*
	((low-chars "abcdefghijklmnopqrstuvwxyz")
	 (low-chars-list (coerce low-chars 'list))
	 (high-chars-list (coerce (upcase low-chars) 'list))
	 (both-list (append low-chars-list high-chars-list)))
  (defvar/fun =alpha
	(=>satisfies 
	 (lambda (x) (in (to-char x) both-list)))
	"Parse an alphabetical character.")
  (defvar/fun =alpha-upper 
	(=>satisfies
	 (lambda (x) (in (to-char x) high-chars-list))) 
	"Parse an uppercast alphabetical character.")
  (defvar/fun =alpha-lower
	(=>satisfies 
	 (lambda (x) (in (to-char x) low-chars-list)))
	"Parse a lowercase alphabetical character"))

(lexical-let 
	((digits (coerce "1234567890" 'list)))
  (defvar/fun =digit 
	(=>satisfies 
	 (lambda (x) (in (to-char x) digits)))))

(defun/var =input-type (input)
  "Monadically return the type of the input being parsed."
  (list (cons (input-dispatcher input) input)))

(defun =>string (&rest s)
  "Produces a parser which, if the input is a string or buffer,
matches N characters matching the string S.  If the input is a list, then it
succeeds only when the item is a string which matches S."
  (if (= 1 (length s))
	  (lexical-let* ((s (car s))
					 (n (length s)))
		(monadic-do
		 monad-parse
		 (type <- =input-type)
		 (if (eq :list type)
			 (monadic-do
			  monad-parse
			  (item <- =item)
			  (if (and (stringp item) (string= item s))
				  (m-return item)
				=nil))
		   (monadic-do 
			monad-parse
			(items <- (=>items->string n))
			(if (string= items s) (m-return items)
			  =nil)))))
	(apply #'=>or (mapcar #'=>string s))))

(defun =>stringi (&rest s)
  "Produces a parser which, if the input is a string or buffer,
matches N characters matching the string S.  If the input is a list, then it
succeeds only when the item is a string which matches S."
  (if (= 1 (length s))
	  (lexical-let* ((s (car s))
					 (n (length s)))
		(monadic-do
		 monad-parse
		 (type <- =input-type)
		 (if (eq :list type)
			 (monadic-do
			  monad-parse
			  (item <- =item)
			  (if (and (stringp item) (stringi= item s))
				  (m-return item)
				=nil))
		   (monadic-do 
			monad-parse
			(items <- (=>items->string n))
			(if (stringi= items s) (m-return items)
			  =nil)))))
	(apply #'=>or (mapcar #'=>stringi s))))

(defvar/fun =number-from-list 
  (monadic-do 
   monad-parse
   (item <- =item)
   (if (numberp item) (m-return item) =nil))
  "Parse a number from a list.  This parser will always fail on
character based inputs.")

(defvar/fun =sign 
  (=>or (=>string "+") (=>string "-")))

(defvar/fun =string-of-digits 
  (=>zero-plus-more =digit))

(defvar/fun =string-of-digits->string 
  (monadic-do
   monad-parse
   (items <- =string-of-digits)
   (m-return 
	(if (empty? items) "" (reduce #'concat items)))))

(defvar/fun =dot (=>string "."))

(defvar/fun =number-char
  (monadic-do
   monad-parse
   (sign <- (=>maybe =sign))
   (pre  <- =string-of-digits->string)
   (dot <- (=>maybe =dot))
   (rest <- =string-of-digits->string)
   (m-return
	(string-to-number
	 (let ((sign (if sign sign "")))
	   (if dot (concat sign pre dot rest)
		 (concat sign pre)))))))

(defvar/fun =number
  (monadic-do 
   monad-parse
   (input-type <- =input-type)
   (if (eq input-type :list)
	   =number-from-list
	 =number-char)))

(defmacro parser (&rest body)
  "Create a parser by using the parser monad to sequence the
monadic values represented by the forms in BODY.  Each form must
either be a monadic value or a binding form of the type (SYMBOL
<- EXPR), where the expression is a monadic value.  SYMBOL is
bound to the monadic return value thereof in subsequent
expressions."
  `(monadic-do monad-parse
			   ,@body))

(defmacro defparser-fun (name args maybe-doc &rest body)
  "Create a parser-producing function by evaluating the BODY
expressions as in a MONADIC-DO in the body of a function named
NAME with lexically bound arguments ARGS.  If MAYBE-DOC is a string,
it is counted as the doc-string for the function."
  `(lex-defun ,name ,args
	 ,@(if (stringp maybe-doc) (list maybe-doc) nil)
	 (parser
	  ,@(if (not (stringp maybe-doc)) (list maybe-doc) nil)
	  ,@body)))

(defmacro defparser-val (name maybe-doc &rest body)
  "Creates a parser value/function binding by evaluating the BODY
as if in a monadic-do form in the parser monad.  If MAYBE-DOC is
a string, this is treated as the doc-string."
  `(defvar/fun ,name
	 (parser ,@(if (not (stringp maybe-doc)) (list maybe-doc) nil)
			 ,@body)
	 ,@(if (stringp maybe-doc) (list maybe-doc) nil)))

(defmacro defparser (name/args maybe-doc &rest body)
  "Combines DEFPARSER-VAL and DEFPARSER-FUN in one easy to use,
Scheme-like defining form.  If NAME/ARGS is a symbol, this
expression defines a parser as in DEFPARSER-VAL.  If it is a list,
then the car of the list is taken to be the function name and the
CDR the arguments in a DEFPARSER-FUN expression."
  (if (symbolp name/args) `(defparser-val ,name/args ,maybe-doc ,@body)
	(let ((name (car name/args))
		  (args (cdr name/args)))
	  `(defparser-fun ,name ,args ,maybe-doc ,@body))))

(defparser (=>this-symbol s) 
  "Return a parser matching S only.  Such a parser always fails
for string and buffer input."
  (item <- =item)
  (if (and (symbolp item) (eq item s)) 
	  (m-return item)
	=nil))

(lexical-let 
	((punctuation 
	  (coerce "~`!@#$%^&*()_-+={[}]|\\/?.,<>;:'\"" 'list)))
  (defparser =punctuation 
	"Matches any punctuation mark."
	(item <- =item)
	(if (in (to-char item) punctuation) 
		(m-return item)
	  =nil)))

(defparser (=>equal what)
  "Returns a parser which succeeds when an ITEM is EQUAL to WHAT."
  (item <- =item)
  (if (equal item what) (m-return item) =nil))

(defparser (=>eq what)
  "Returns a parser which succeeds when an ITEM is EQ to WHAT."
  (item <- =item)
  (if (eq item what) (m-return item) =nil))

(defparser (=>n-equal n to)
  "Returns a parser which succeeds when N ITEMS are equal to the list TO."
  (items <- (=>items n))
  (if (equal items to)
	  (m-return items)
	=nil))

(defun =>unparse (what)
  "An unparser: put WHAT onto the input."
  (enclose 
   (what)
   (lambda (input)
	 (list (cons what
				 (input-push what input))))))

(defun/var =peek (input)
  "Return the next ITEM without removing it from the input."
  (list (cons (input-first input) input)))

(defparser (=>items-upto predicate)
  "Collect items until PREDICATE is true for one, which is left on the input."
  (peek <- =peek)
  (if (funcall predicate peek) (m-return nil)
	(parser
	 (item <- =item)
	 (rest <- (=>items-upto predicate))
	 (m-return (cons item 
					 rest)))))

(defparser (=>items-upto->string predicate)
  "As =>ITEMS-UPTO but concatenates list of results."
  (=>reduce-concat (=>items-upto predicate)))

(defun =input (input)
  "Get the input state as it is."
  (list (cons input input)))

(defun =>set-input (to)
  "Set the input state to the value TO.  Monadically return T."
  (enclose (to)
		   (lambda (input)
			 (list (cons t to)))))

(provide 'better-monad-parse)