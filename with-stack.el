(require 'utils)
(require 'macro-utils)
(require 'cl)

(defvar *stack* nil "Should always be nil, defined so the compiler shuts up about it.")
(defvar *retain-stack* nil "Should always be nil, defined so the compiler shuts up about it.")



(defun stack-atomp (item)
  "Detects whether an item is a stack atom."
  (or (numberp item)
	  (stringp item)
	  (vectorp item)
	  (quotep item)))

(defun handle-stack-atom (item)
  "Generates code for a stack atom."
  (cond ((quotep item) `(push ,item *stack*))
		(t `(push ,item *stack*))))

(defvar *stack-words* (tbl!) "Dictionary of stack words.")

(defun push-stack (x)
  "Pushes an item on the dynamically scoped stack."
  (push x *stack*))
(defun pop-stack ()
  "Pops an item off the dynamically scoped stack."
  (pop *stack*))

(defun stack-wordp (item)
  "Returns true if item is a symbol representing a stack word."
  (and (symbolp item)
	   ($ item in *stack-words*)))

(defmacro* defstackword (name &body body)
  "Define a new stack word and insert it into the stack word dictionary."
  (let ((actual-name (internf "stack-%s-" name)))
	`(eval-when-compile-also  
	   (defun ,actual-name () ,@body)
	   (tbl! *stack-words* ',name (list ',actual-name nil))
	   (byte-compile ',actual-name))))

(defmacro* defstackword-immediate (name &body body)
  "Define a new stack word and insert it into the stack word dictionary, and mark it as immediate."
  (let ((actual-name (internf "stack-%s-" name)))
	`(eval-when-compile-also 
	   (defun ,actual-name () ,@body)
	   (tbl! *stack-words* ',name (list ',actual-name t)) 
	   (byte-compile ',actual-name)
	   )))

(defun stackword-immediatep (word)
  "Returns true when WORD is a symbol representing an immediate stackword."
  (cadr (tbl *stack-words* word)))

(defun handle-immediate-stackword (item)
  "Calls the code representing the stack word ITEM right now."
  (let ((*stack* code)
		(*retain-stack* nil))
	(funcall (car (tbl *stack-words* item)))
	(setf code *stack*))
  nil)

(defun handle-stack-word (item)
  "Generates the code to call a stack word."
  (if (stackword-immediatep item) (handle-immediate-stackword item)
	`(,(car (tbl *stack-words* item)))))

(defun stack-emacs-callp (item)
  "Detects a call-to-emacs syntax like N>function."
  (let ((s (format "%s" item)))
	(and ($ ">" in s)
		 (let* ((parts (split-string s ">"))
				(n-stack-part (car parts)))
		   (or (string= n-stack-part "n")
			   (numberp (read n-stack-part)))))))

(defun gen-temp-syms (n)
  "Generate a set of temporary symbols for with-stack."
  (loop for i from 1 to n collect 
		(gensym (format "stack-temp-%d" i))))

(defun pop-n (n where)
  "Returns the first N items from WHERE, removes them from WHERE."
  "returns a list of the first n items WHERE, which are removed"
  (loop for i from 1 to n collect (pop where)))

(defun handle-emacs-call (item)
  "Generates the code for a call to an emacs function."
  (let* ((s (format "%s" item))
		 (parts (split-string s ">"))
		 (n (read (car parts)))
		 (sym (intern (join (cdr parts) ">")))
		 (list-sym (gensym "npop-"))
		 (temp-syms (if (numberp n) (gen-temp-syms n) nil)))

	(cond ((and (symbolp n) (eq n 'n))
		   `(let ((,list-sym (pop-n (pop *stack*) *stack*)))
			  (apply #',sym (reverse ,list-sym))))
		  ((numberp n)
		   `(let ,(loop for s in temp-syms collect
						`(,s (pop *stack*)))
			  (push (,sym ,@(reverse temp-syms)) *stack*))))))

(defun stack-interpolationp (item)
  "Detects stack interpolation syntax, that is when a symbol is bracketed without spaces: {symbol}."
  (let* ((s (format "%s" item))
		 (n (length s)))
	(and (string= (substring s 0 1) "{")
		 (string= (substring s (- n 1) n) "}"))))

(defun stack-get-interpolation-symbol (item)
  "Reads out the symbol value from an interpolated symbol: {x} -> x."
  (let* ((s (format "%s" item))
		 (n (length s)))
	(intern (substring s 1 (- n 1)))))

(defun handle-stack-interpolation (item)
  "Generates the code to handle with-stack symbol interpolation."
  `(push ,(stack-get-interpolation-symbol item) *stack*))

(defun handle-stack-symbol (item)
  "Main workhorse for with-stack, dispatches handling to appropriate handler to generate code."
  (cond 
   ((or (eq item 't) (eq item t)) `(push t *stack*))
   ((keywordp item) `(push ,item *stack*))
   ((stack-wordp item) (handle-stack-word item))
   ((stack-emacs-callp item) (handle-emacs-call item))
   ((stack-interpolationp item) (handle-stack-interpolation item))
   (t (error (format "stack: Can't figure out how to compile %s." item)))))

(defmacro* with-stack- (&body code)
  "Evaluate the code CODE with the stack language, using the dynamicly bound *stack*.  Return the top of the stack."
  `(progn
	 ,@(filter #'identity (loop while code collect
								(let ((item (car code)))
								  (setf code (cdr code)) 
								  (cond 
								   ((eq item nil)
									`(push nil *stack*))
								   ((stack-atomp item)
									(handle-stack-atom item))
								   ((symbolp item)
									(handle-stack-symbol item))))))
	 (car *stack*)))

(defmacro* with-stack (&body code)
  "Evaluate the code CODE with a fresh, empty stack.  Return top of stack."
  `(let ((*stack* nil)
		 (*retain-stack* nil))
	 ,@(filter #'identity (loop while code collect
								(let ((item (car code)))
								  (setf code (cdr code)) 
								  (cond 
								   ((eq item nil)
									`(push nil *stack*))
								   ((stack-atomp item)
									(handle-stack-atom item))
								   ((symbolp item)
									(handle-stack-symbol item))))))
	 (car *stack*)))

(defmacro* ||| (&body body)
  "Synonym for WITH-STACK."
  `(with-stack ,@body))
(defmacro* |||- (&body body)
  "Synonym for WITH-STACK-"
  `(with-stack- ,@body))
(defmacro* |||p (&body body)
  "Like WITH-STACK but prints the stack before returning the top of the stack."
  `(with-stack ,@body print-stack))

(defun stack-at-least (n)
  "Checks the stack depth is at least n."
  (>= (length *stack*) n))

(defun retain-stack-at-least (n)
  "Check the retain stack depth is at least n."
  (>= (length *retain-stack*) n))

(defmacro bivalent-stack-word (s)
  "Tell the stack language that the emacs function represented by S can be called as a stack word with two arguments."
  `(defstackword ,s 
	 (|||- ,(intern (format "2>%s" s)))))

(defmacro bivalent-stack-words (&rest ss)
  "Declare that the emacs functions in SS are all two argument stack words."
  `(progn ,@(loop for s in ss collect 
				  `(bivalent-stack-word ,s))))

(defmacro univalent-stack-word (s)
  "Tell the stack language that S is a stack word which consumes one argument."
  `(defstackword ,s 
	 (|||- ,(intern (format "1>%s" s)))))

(defmacro univalent-stack-words (&rest ss)
  "Tell the stack language that all the emacs functions in SS are single argument stack words."
  `(progn ,@(loop for s in ss collect 
				  `(univalent-stack-word ,s))))

(defmacro n-valent-stack-word (n s)
  "Tell the stack language that emacs function S can be called as a stack word with N arguments."
  `(defstackword ,s 
	 (|||- ,(intern (format "%d>%s" n s)))))

(defmacro n-valent-stack-words (n &rest ss)
  "Tell the stack language that all of the emacs functions SS can be called with N arguments."
  `(progn ,@(loop for s in ss collect
				  `(n-valent-stack-word ,n ,s))))



(defstackword print (print (pop *stack*)))
(defstackword call
  (let ((qtn (pop *stack*)))
	(if (not (listp qtn)) (error "stack: call can't call something that isn't a list.")
	  (eval `(with-stack- ,@qtn)))))
(defstackword if 
  (if (stack-at-least 3)
	  (let* ((false-branch (pop *stack*))
			 (true-branch (pop *stack*))
			 (predicate (pop *stack*)))
		(if (and (listp false-branch)
				 (listp true-branch))
			(eval `(with-stack- ,@(if predicate true-branch false-branch)))
		  (error "stack: if requires two quotations on the stack.")))
	(error "stack: if needs at least three elements on the stack.")))

(defstackword loop 
  (let* ((qtn (pop *stack*))
		 (code (macroexpand-all `(with-stack- ,@qtn))))
	(eval code)
	(loop while (pop *stack*) do (eval code))))

(defstackword drop 
  (if *stack* (pop *stack*) (error "stack: stack underflow (from drop)")))
(defstackword swap 
  (if (stack-at-least 2)
	  (let ((top (pop *stack*))
			(new-top (pop *stack*)))
		(push top *stack*)
		(push new-top *stack*))
	(error "stack: swap needs at least two elements on the stack.")))
(defstackword curry 
  (if (stack-at-least 2)
	  (let ((qtn (pop *stack*))
			(item (pop *stack*)))
		(if (listp qtn)
			(if (atom item)
				(push (cons item qtn) *stack*)
			  (push (cons `(quote ,item) qtn) *stack*))
		  (error "stack: curry needs a list on the top of the stack.")))
	(error "stack: curry needs at least two arguments on the stack.")))
(defstackword compose 
  (if (stack-at-least 2)
	  (let ((qtn1 (pop *stack*))
			(qtn2 (pop *stack*)))
		(if (and (listp qtn1)
				 (listp qtn2))
			(push (append qtn2 qtn1) *stack*)
		  (error "stack: compose needs two lists on the stack.")))
	(error "stack: compose needs at least two arguments on the stack.")))
(defstackword dup 
  (push (car *stack*) *stack*))
(defstackword nip 
  (with-stack- swap drop))
(defstackword print-stack 
  (print (concat "stack:" (format "\n")
				 (loop 
				  with str = "" 
				  for object in (reverse *stack*) do
				  (setf str (concat str (format "- %s" object) (format "\n")))
				  finally (return str)))))

(defstackword dupd 
  (let ((top (pop *stack*)))
	(push (car *stack*) *stack*)
	(push top *stack*)))
(defstackword over 
  (push (elt *stack* 1) *stack*))
(defstackword pick
  (push (elt *stack* 2) *stack*))

(defstackword dip 
  (if (stack-at-least 1)
	  (let ((qtn (pop-stack))
			(hold (pop-stack)))
		(push-stack qtn)
		(|||- call)
		(push-stack hold))
	(error "stack: dip requires at least a quotation on the stack.")))

(defstackword swapd 
  (if (stack-at-least 3)
	  (let ((top (pop-stack)))
		(|||- swap)
		(push-stack top))
	(error "stack: swapd requires at least three items on the stack.")))

(defstackword rot 
  (if (stack-at-least 3)
	  (|||- swapd swap)
	(error "stack: rot requires at least three items on the stack.")))

(defstackword -rot 
  (if (stack-at-least 3)
	  (|||- swap swapd)
	(error "stack: -rot requires at least three items on the stack.")))

(defstackword tuck
  (if (stack-at-least 2)
	  (|||- swap over)))

(defstackword >r 
  (if (stack-at-least 1)
	  (push (pop-stack) *retain-stack*)
	(error "stack: >r requires at least something on the stack.")))

(defstackword r>
  (if (retain-stack-at-least 1)
	  (push-stack (pop *retain-stack*))
	(error "stack: r> requires at least one item on the retain stack.")))

(defstackword push-list 
  (let ((list (pop-stack)))
	(loop for el in list do
		  (push-stack el))))

(defstackword keep
  (|||- over '(call) dip))

(defstackword bi 
  (|||- '(keep) dip call))

(defstackword + 
  (|||- 2>+))
(defstackword -
  (|||- 2>-))
(defstackword *
  (|||- 2>*))
(defstackword /
  (|||- 2>/))
(defstackword and 
  (|||- 2>and))
(defstackword or
  (|||- 2>or))
(bivalent-stack-word >)
(bivalent-stack-word <)
(bivalent-stack-word >=)
(bivalent-stack-word <=)
(bivalent-stack-word =)
(bivalent-stack-word eq)
(bivalent-stack-word equal)



(defstackword stack 
  (push-stack *stack*))

(defstackword retain-stack
  (push-stack *retain-stack*))

(defstackword bi*
  (|||- '(dip) dip call))

(defstackword bi@
  (|||- dup bi*))

(defstackword 2drop
  (|||- drop drop))

;; (defun fill-in-fry (qtn)
;;   "Take a quotation and fill in the fry spaces (_ and @) with values from the stack."
;;   (reverse (loop for item in (reverse qtn) collect
;; 				 (cond 
;; 				  ((and (symbolp item)
;; 						(eq item '_))
;; 				   (pop-stack))
;; 				  ((listp item)
;; 				   (mapcar #'fill-in-fry item))
;; 				  (t item)))))

(defun fill-in-fry (future)
  "Take a quotation and fill in the fry spaces (_ and @) with values from the stack."
  (reverse (fill-in-fry-natural (reverse future) nil)))

(defun fill-in-fry-natural (future past)
  "Fill in the fry words in a more natural order, trust a higher function to reverse all sublists."
  (if (not future) past
	(let* ((item (car future))
		   (rest (cdr future))
		   (more-past 
			(cond ((symbolp item)
				   (cond ((eq '_ item) (append past (list (pop-stack))))
						 ((eq '@ item) (append past (pop-stack)))
						 (t (append past (list item)))))
				  ((listp item) (append past (list (fill-in-fry-natural item nil))))
				  (t (append past (list item))))))
	  (fill-in-fry-natural rest more-past))))

(defstackword fry 
  (assert (stack-at-least 1) "stack: fry needs at least a quotation on the stack.")
  (let ((partial-quotation (pop-stack)))
	(push-stack (fill-in-fry partial-quotation))))

(defstackword set-word! 
  (assert (stack-at-least 2) "stack: set-word requires two arguments on the stack.")
  (let ((name (pop-stack))
		(body (pop-stack)))
	(assert (symbolp name) "stack: top of stack must be a symbol in set-word!.")
	(assert (listp body) "stack: second argument of set-word! must be a quotation.")
	(eval `(defstackword ,name (|||- ,@body)))))

(defstackword-immediate word: 
  (let* ((name (pop-stack))
		 (body 
		  (loop while (not (eq 'end: (car *stack*))) 
				collect (prog1 
							(pop-stack)
						  (if (= 0 (length *stack*)) (error (format "stack: Couldn't find the end of the stack-word: %s" name)))))))
	(pop-stack)
	(eval `(defstackword ,name (|||- ,@body)))
	(eval `(byte-compile ',(internf "stack-%s-" name)))
	(push-stack '1>eval)
	(push-stack `(quote (byte-compile ',(internf "stack-%s-" name))))
	(push-stack '1>eval)
	(push-stack `(quote (defstackword ,name (|||- ,@body))))
	))

(defstackword-immediate immediate-word: 
  (let* ((name (pop-stack))
		 (body 
		  (loop while (not (eq 'end: (car *stack*))) 
				collect (prog1 
							(pop-stack)
						  (if (= 0 (length *stack*)) (error (format "stack: Couldn't find the end of the stack-word: %s" name)))))))
	(pop-stack)
	(eval `(defstackword-immediate ,name (|||- ,@body)))
	(eval `(byte-compile ',(internf "stack-%s-" name)))
	(push-stack '1>eval)
	(push-stack `(quote (byte-compile ',(internf "stack-%s-" name))))
	(push-stack '1>eval)
	(push-stack `(quote (defstackword-immediate ,name (|||- ,@body))))
	))

(defmacro assert-stack-predicates (predicates word-name)
  "Takes a list of PREDICATES and a WORD-NAME for error message generation, and checks each item on the stack in the same order as predicates.  Generate an error on failure."
  `(progn 
	 ,@(loop for pred in (reverse predicates)
			 and i from 0 collect
			 `(if (not (,pred (elt *stack* ,i))) (error ,(format 
														  "stack: stack element %d must pass predicate %s in %s"
														  i
														  pred
														  word-name))))))

(defun stack-quotationp (x)
  "Detect a stack quotation (synonymous with listp)"
  (listp x))

(defstackword assert-stack-predicates 
  (assert-stack-predicates (listp symbolp) assert-stack-predicates)
  (let ((name (pop-stack))
		(predicates (pop-stack)))
	(eval `(assert-stack-predicates ,predicates ,name))))

(defstackword leach 
  (assert (stack-at-least 2) "stack: leach requires at least two items on the stack.")
  (assert-stack-predicates (listp stack-quotationp) leach)
  (let ((qtn (pop-stack))
		(seq (pop-stack)))
	(loop for item in seq do 
		  (push-stack item)
		  (push-stack qtn)
		  (|||- call))))



(defstackword-immediate lisp-val: 
  (assert (stack-at-least 1) "stack: lisp-val: needs one word after it, at least.")
  (let ((expr (pop-stack)))
	(push-stack '1>eval)
	(push-stack `(quote ,expr))))

(defstackword 2dip
  (assert (stack-at-least 3) "stack: 2dip needs at least three arguments on the stack.")
  (assert-stack-predicates (stack-quotationp) '2dip)
  (let ((q (pop-stack))
		(a (pop-stack))
		(b (pop-stack)))
	(push-stack q)
	(|||- call)
	(push-stack b)
	(push-stack a)))

(defstackword rearrange ;( ... arrangement - ... )
  (assert-stack-predicates (stack-quotationp) 'rearrange)
  (let* ((arrangement (pop-stack))
		 (to-drop (+ 1 (apply #'max arrangement))))
	(assert (stack-at-least to-drop) (format "stack: rearrange needs a stack with enough elements to re-arrange."))
	(let ((new-vals (elts *stack* arrangement)))
	  (loop for i from 1 to to-drop do (pop-stack))
	  (setf *stack* (append (reverse new-vals) *stack*)))))

;; (defstackword swipe ;( ... item qtn -- ... item ... )
;;   (assert-stack-predicates (stack-quotationp) 'swipe)
;;   (assert (stack-at-least 2) "stack: swipe needs at least two elements on the stack")
;;   (let ((qtn (pop-stack))
;; 		(item (pop-stack))
;; 		(depth (length *stack*))
;; 		(holder nil))
;; 	(push-stack qtn)
;; 	(|||- call)
;; 	(let* ((new-depth (length *stack*))
;; 		   (n-new (- new-depth depth)))
;; 	  (if (>= n-new 0) 
;; 		  (progn (loop for i from 1 to n-new do
;; 					   (push (pop-stack) holder))
;; 				 (push-stack item)
;; 				 (loop for i from 1 to n-new do
;; 					   (push-stack (pop holder))))))))

(defstackword filter ;( lst quot -- lst )
  (assert (stack-at-least 2) "stack: filter requires 2 items on the stack.")
  (assert-stack-predicates (stack-quotationp listp) 'filter)
  (let ((qtn (pop-stack))
		(lst (pop-stack)))
	(push-stack (loop for i in lst when 
					  (prog1 (|||- {i} {qtn} call)
						(pop-stack))
					  collect i))))

(defstackword in ;( item lst -- bool )
  (|||- 2>in))

(univalent-stack-words car cdr cadr first second third fourth list regexp-quote rxq reverse length)
(bivalent-stack-words split-string join cons)
(n-valent-stack-words 3 replace-regexp-in-string reprxstr substring)

(defstackword apply-emacs-fun 
  (let ((arg-list (pop *stack*))
		(fun (pop *stack*)))
	(push (apply fun arg-list) *stack*)))

(defmacro fn1||| (&rest rest)
  (with-gensyms (lambda1-|||arg) 
				`(lambda (,lambda1-|||arg) (||| lisp-val: ,lambda1-|||arg ,@rest))))

(defmacro fn2||| (&rest rest)
  (with-gensyms (a1 a2) 
				`(lambda (,a1 ,a2) (||| lisp-val: ,a1 lisp-val: ,a2 ,@rest))))

(defmacro fn3||| (&rest rest)
  (with-gensyms (a1 a2 a3) 
				`(lambda (,a1 ,a2 ,a3) (||| lisp-val: ,a1 lisp-val: ,a2 lisp-val: ,a3 ,@rest))))

(defmacro fn*||| (&rest rest)
  (with-gensyms (args) 
				`(lambda (&rest ,args) (||| lisp-val: ,args push-list ,@rest))))

(defmacro* word: (&body body)
  `(||| word: ,@body end:))

(defun |||-region (st en)
  (interactive "r")
  (eval `(||| ,@(read (concat "(" (buffer-substring st en) ")")))))


(provide 'with-stack)
