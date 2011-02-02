(require 'lisp-parser)
(require 'functional)
(require 'with-stack)
(require 'stack-words)

(defunc =microstack-symbol ()
  (=let* [_ (=or (letter)
				 (=space)
				 (=other-id-char))]
		 (if _ 
			 (intern (concat (list _))) nil)))


(defun microstack-parser () (zero-or-more (=or
										   (=microstack-symbol)
										   (=number)
										   (=lisp-string))))

(defun parse-microstack (code)
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
	(push (translate-microstack (parse-microstack str)))))
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
  (||| lisp-val: (point) lisp-val: (mark) 2>kill-region))

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
	   'g '1>goto-char
	   'x 'kill-current-region
	   '* '*
	   '/ '/
	   's '1>search-forward
	   'S '1>search-forward-regexp
	   'c 'concat
	   'i 'insert))

(defun translate-microstack (code)
  (loop for el in code append
		(cond 
		 ((symbolp el)
		  (let ((trans (alist micro-stack-map el)))
			(if trans (list trans) (error "Unknown microstack word."))))
		 (t (list el)))))

(defun do-microstack-parsed-translated (code)
  (eval `(||| ,@code)))

(defun do-microstack (str)
  (interactive "s")
  (let* ((code (parse-microstack str))
		 (code (translate-microstack code)))
	(print code)
	(do-microstack-parsed-translated code)))

