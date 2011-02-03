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


(defun microstack-parser ()
  "Parser for the microstack language."
  (zero-or-more (=or
				 (=microstack-symbol)
				 (=number)
				 (=lisp-string))))

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

(setq micro-stack-map 
	  (alist>> 
	   'b '0>backward-char
	   'B '1>backward-char
	   'f '0>forward-char
	   'F '1>forward-char
	   'd 'delete-forward0
	   'D 'delete-forward
	   'k 'delete-backward0
	   'K 'delete-backward
	   'q 'microstack->quotation
	   'Q 'string->quotation
	   '! 'call
	   '? 'if
	   '+ '+
	   '- '-
	   't 't
	   '_ 'nil
	   'm '0>push-mark
	   'M '0>mark ; put the mark position on the stack
	   'g '1>goto-char
	   'x 'kill-current-region
	   '* '*
	   '/ '/
	   '= '2>equal
	   'N 'do-n-times
	   'L 'loop
	   's '1>search-forward
	   'S '1>search-forward-regexp
	   'c 'concat
	   (intern ",") 'print-stack
	   (intern ":") 'dup
	   '@ 'char-at-point->string
		  (intern ".") 'print
		  'U 'loop-until
		  'W 'loop-while
		  'i 'insert))

(defun translate-microstack (code)
  "Translate the single character symbols to their stack words.  Process special microstack behavior words."
  (loop for el in code append
		(cond 
		 ((symbolp el)
		  (let ((trans (alist micro-stack-map el)))
			(if trans (list trans) (error "Unknown microstack word."))))
		 (t (list el)))))

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

