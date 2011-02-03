(require 'lisp-parser)
(require 'functional)
(require 'with-stack)
(require 'stack-words)
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

(defunc =bracketed-string ()
  (=let* [_ (=char ?\[)
			contents (zero-or-more (=or 
									(=escaped-close-bracket)
									(=satisfies
									 (lex-lambda (c) (!= c ?\])))))
			_ (=char ?\])]
		 (parse-and-translate-microstack (coerce (flatten contents) 'string))))


(defun microstack-parser ()
  "Parser for the microstack language."
  (zero-or-more (=or
				 (=microstack-symbol)
				 (=number)
				 (=lisp-string)
				 (=bracketed-string))))

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

(defstackword forward 
  (forward-char))
(defstackword backward
  (backward-char))

(defstackword format 
  (let ((fmtstr (pop *stack*))
		(rest (pop *stack*)))
	(push (apply #'format (cons fmtstr rest)) *stack*)))

(setq micro-stack-map 
	  (alist>> 
	   'b 'backward ; move the point backward once
	   'B '1>backward-char ; move the point backward n times, pop n from the stack
	   'f 'forward ; move the point forward once
	   'F '1>forward-char ; move the point forward n times, pop n from the stack
	   'd 'delete-forward0 ; delete forward once
	   'D 'delete-forward ; delete forward n times, pop n from the stack
	   'k 'delete-backward0 ; delete backward once
	   'K 'delete-backward ; delete backward n times, remove n from the stack
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
	   (intern ",") 'print-stack ; print the stack
	   (intern ":") 'dup ; dup
	   (intern "$") 'swap ; swap the top two stack elements
	   (intern "#") 'length ; pop object off the stack and push its length
	   (intern "@") 'char-at-point->string ;push the current character onto the stack
	   (intern ".") 'print ; print the top of the stack, pop it
	   (intern "%") 'format ; lst format-string format; calls format with the string format-string and lst as rest args
	   'U 'loop-until ; qt pred loop-until ; loop qt until pred is true
	   'W 'loop-while ; qt pred loop-while ; loop qt while pred is true
	   'i 'insert ; insert the top of the stack as text into the buffer
))



(defun translate-microstack (code)
  "Translate the single character symbols to their stack words.  Process special microstack behavior words."
  (loop for el in code append
		(cond 
		 ((symbolp el)
		  (let ((trans (alist micro-stack-map el)))
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
	(print code)
	(do-microstack-parsed-translated code)))

