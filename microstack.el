(require 'lisp-parser)
(require 'functional)
(require 'with-stack)
(require 'stack-words)
(require 'multi-methods)
(provide 'microstack)



(defunc =microstack-symbol ()
  "Parser for a microstack symbol, or a space (no-op)."
  (=let* [_ (=or (letter)
				 (=space)
				 (=other-id-char))]
		 (if _ 
			 (intern (concat (list _))) nil)))

(defunc =escaped-close-bracket ()
  "Parse an escaped close bracket."
  "["
  (=let* [_ (=string "\\]")]
		 (if _ ?\" nil)))

;; (defunc =bracketed-string ()
;;   (=let* [_ (=char ?\[)
;; 			contents (zero-or-more (=or 
;; 									(=escaped-close-bracket)
;; 									(=satisfies
;; 									 (lex-lambda (c) (!= c ?\])))))
;; 			_ (=char ?\])]
;; 		 (parse-and-translate-microstack (coerce (flatten contents) 'string))))

(defunc =single-quote-string ()
  (=let* [_ (=char ?\')
			contents (zero-or-more (=or 
									(=escaped-close-bracket)
									(=satisfies
									 (lex-lambda (c) (!= c ?\')))))
			_ (=char ?\')]
		 (coerce (flatten contents) 'string)))

(defun =microstack-string ()
  (=or (=single-quote-string)
	   (=lisp-string)))


(defun =microstack-quote ()
  (=let* [_ (=char ?\[)
			contents (microstack-parser)
			_ (=char ?\])]
		 (translate-microstack contents)))

(defun microstack-parser ()
  "Parser for the microstack language."
  (zero-or-more (=or
				 (=number)
				 (=microstack-symbol)
				 (=microstack-string)
				 (=microstack-quote))))

(defun parse-microstack (code)
  "Parse the microstack language and return the results as a sequence of symbols, numbers, strings.  Remove no-ops."
  (filter
   (f-not (cr #'eq (intern " ")))
   (car (parse-string-det (microstack-parser) code))))

(defstackword delete-forward0 
  (backward-delete-char -1))
(defstackword delete-backward0
  (backward-delete-char 1))
(defstackword delete-backward 
  (backward-delete-char (pop *stack*)))
(defstackword delete-forward
  (backward-delete-char (- (pop *stack*))))
(defstackword insert (insert (pop *stack*)))
(defstackword microstack->quotation
  (let ((str (pop *stack*)))
	(push (translate-microstack (parse-microstack str)) *stack*)))
(defstackword do-n-times 
  (let ((n (pop *stack*))
		(q (pop *stack*)))
	(loop for i from 0 below n do
		  (|||- {q} call))))

(defstackword call-string 
  (|||- 1>intern 1>list call))

(defstackword string->quotation 
  (|||- 1>intern 1>list))

(defstackword kill-current-region 
  (|||- lisp-val: (point) lisp-val: (mark) 2>kill-region))

(defstackword char-at-point->string
  (push (buffer-substring-no-properties (point) (+ 1 (point))) *stack*))

(defstackword loop-while
  (let ((con (pop *stack*))
		(qtn (pop *stack*)))
	(loop do
		  (|||- {con} call)
		  while (pop *stack*)
		  do
		  (|||- {qtn} call))))
(defstackword loop-until
  (let ((con (pop *stack*))
		(qtn (pop *stack*)))
	(loop do
		  (|||- {con} call)
		  while (not (pop *stack*))
		  do
		  (|||- {qtn} call))))

(defstackword loop-until-char 
  (|||- '(char-at-point->string 2>string=) curry loop-until))

(defstackword forward 
  (forward-char))
(defstackword backward
  (backward-char))

(defstackword format 
  (let ((fmtstr (pop *stack*))
		(rest (pop *stack*)))
	(push (apply #'format (cons fmtstr rest)) *stack*)))

(defun move-dispatcher (object)
  "Dispatch for generic motion."
  (cond ((numberp object) :char)
		((listp object) (car object))
		(t nil)))

(defmulti move #'move-dispatcher "A generic motion function.")

(defunmethod move :char (movement)
  (let ((n (if 
			   (listp movement) (cadr movement)
			 movement)))
	(forward-char n)))

(defunmethod move :word (movement)
  (forward-word (cadr movement)))

(defunmethod move :line (movement)
  (beginning-of-line)
  (forward-line (cadr movement)))

(defunmethod move :paragraph (movement)
  (forward-paragraph (cadr movement)))

(defstackword word 
  (|||- :word swap 2>list))

(defstackword paragraph 
  (|||- :paragraph swap 2>list))

(defstackword page
  (|||- :page swap 2>list))

(defstackword line 
  (|||- :line swap 2>list))

(defstackword char 
  (|||- :char swap 2>list))

(defstackword sym 
  (|||- :symbol swap 2>list))

(defstackword s-expression 
  (|||- :sexp swap 2>list))

(defstackword make-quantity-of 
  (|||- 1>make-keyword swap 2>list))

($ :char		derives-from :movement-type)
($ :movement-type-with-extent 
   derives-from :movement-type)
($ :word		derives-from :movement-type-with-extent)
($ :paragraph	derives-from :movement-type-with-extent)
($ :line		derives-from :movement-type-with-extent)


(defmulti move-kill #'move-dispatcher "Generic deletion method.")

(defmulti pre-delete-movement (function move-dispatcher) "Handle movement before delete.")
(defunmethod pre-delete-movement :movement-type (movement)
  nil
  )

(defmulti post-delete-movement (function move-dispatcher) "Handle movement before delete.")
(defunmethod post-delete-movement :movement-type (movement)
  nil
  )

(defun bounds-of-thing-at-point-kw (kw)
  (bounds-of-thing-at-point (keyword->symbol kw)))

(defunmethod pre-delete-movement :movement-type-with-extent (movement)
  (let* ((thing-bounds (bounds-of-thing-at-point-kw (car movement)))
		 (start (car thing-bounds))
		 (stop  (cdr thing-bounds)))
	(cond ((positive? (cadr movement)) (goto-char start))
		  ((negative? (cadr movement)) (goto-char stop))
		  ((zero? (cadr movement)) nil))))

(defunmethod post-delete-movement :movement-type-with-extent (movement)
  (let* ((thing-bounds (bounds-of-thing-at-point-kw (car movement)))
		 (start (car thing-bounds))
		 (stop  (cdr thing-bounds)))
	(cond ((positive? (cadr movement)) (goto-char stop))
		  ((negative? (cadr movement)) (goto-char start))
		  ((zero? (cadr movement)) nil))))


(defun point-in-word? ()
  (save-excursion (let ((pt (point)))
					(backward-word) (forward-word)
					(!= pt (point)))))

(defunmethod move-kill :movement-type (movement)
  (let (p1 p2)
	(pre-delete-movement movement)
	(setq p1 (point))
	(move movement)
	(post-delete-movement movement)
	(setq p2 (point))
	(kill-region p1 p2)))



(defstackword move (|||- 1>move drop))
(defstackword kill (|||- 1>move-kill drop))

(setq micro-stack-map 
	  (tbl! 
	   'm 'move ; generic movement.  pops an item from the stack, then moves appropriately 
	   'k 'kill ; generic move-and-kil, pops and item of the stack, marks, moves, and kill-region's
	   'l 'line ; specify that a number indicates a number of lines
	   'w 'word ; specify that a number indicates a number of words
	   'y 'sym ; specify that a number indicates a number of symbols
	   'p 'paragraph ; specify that a number indicates a number of paragraphs
	   'P 'page ; specify that a number indicates a number of pages
	   'e 's-expression ; specify that a number indicates a number of s-expressions
	   'G 'make-quantity-of ; take a string and a number and create a general quantity 4"sentence"G -> (:sentence 4)
	   'q 'microstack->quotation ; convert a STRING to a microstack compiled quotation, "..."q is eq to [...]
	   'Q 'string->quotation ;push the stack word represented by string onto the stack to be called later
	   '! 'call ; call a quotation/stack word
	   '? 'if ; if 
	   '+ '+ ; plus
	   '- '- ; -
	   't 't ; push t 
	   '_ 'nil ; push nil
	   'm '0>push-mark ; mark the current point as the mark
	   'M '0>mark ; put the mark position on the stack
	   'g '1>goto-char ; jump to a character number popped from the stack 
	   'x 'kill-current-region ; kill the current region between the point and mark
	   '* '* ; times
	   '/ '/ ; divide
	   '= '2>equal ; equals
	   'N 'do-n-times ; do a quotation n times before stopping
	   'L 'loop ; the loop word in all its general glory - execute a quotation until the top of the stack is true
	   '{ '{{ ; start a list
	   '} '}} ; end a list
	   's '1>search-forward ; search forward for the string on the stack, which is popped
	   'S '1>search-forward-regexp ; search forward for the regex on the stack, which is popped
	   'c 'concat ; concat two strings
	   'o 'rot
	   (intern ",") 'print-stack ; print the stack
	   (intern ":") 'dup ; dup
	   (intern "$") 'swap ; swap the top two stack elements
	   (intern "#") 'length ; pop object off the stack and push its length
	   (intern "@") 'char-at-point->string ;push the current character onto the stack
	   (intern ".") 'print ; print the top of the stack, pop it
	   (intern "%") 'format ; lst format-string format; calls format with the string format-string and lst as rest args
	   (intern "|") 'compose ; compose two quotations
	   (intern "^") 'curry ; curry the value on the stack into the quotation below it.
	   'U 'loop-until ; qt pred loop-until ; loop qt until pred is true
	   'u 'loop-until-char ; qt char loop-until-char; loop qt until char is beneath the cursor.
	   'W 'loop-while ; qt pred loop-while ; loop qt while pred is true
	   'i 'insert ; insert the top of the stack as text into the buffer

))

(defun translate-microstack (code)
  "Translate the single character symbols to their stack words.  Process special microstack behavior words."
  (loop for el in code append
		(cond 
		 ((symbolp el)
		  (let ((trans (tbl micro-stack-map el)))
			(if trans (list trans) (error "Unknown microstack word."))))
		 ((listp el)
		  (list 'lisp-val: `(quote ,el)))
		 (t (list el)))))

(defun parse-and-translate-microstack (code)
  (translate-microstack (parse-microstack code)))

(defun do-microstack-parsed-translated (code)
  "Evaluate the parsed and translated CODE for a microstack statement.  Should be regular stack code at this point."
  (eval `(|||p ,@code)))

(defun do-microstack (str)
  "Parse, translated and execute the microstack code in STR."
  (interactive "s")
  (let* ((code (parse-microstack str))
		 (code (translate-microstack code)))
	(do-microstack-parsed-translated code)))

