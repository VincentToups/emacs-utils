(require 'monads)

(eval-when-compile-also 
 (defun stream-case-names (sub-form)
   (car sub-form))

 (defun stream-case-one (sub-form stream)
   (let ((names (stream-case-names sub-form)))
	 `((not (and (pair? ,stream)
				 (functionp (cdr ,stream))))
	   (let ((,(car names) (car ,stream)))
		 ,@(cdr sub-form)))))

 (defun stream-case-more (sub-form stream)
   (let ((names (stream-case-names sub-form))
		 (body (cdr sub-form)))
	 `(t (let ((,(car names) (car ,stream))
			   (,(cadr names) (cdr ,stream)))
		   ,@body))))

(defmacro stream-case (expr on-zero on-one on-more)
  (with-gensyms 
   (stream)
   `(let ((,stream ,expr))
	  (cond
	   ((not ,stream) ,on-zero)
	   ,(stream-case-one on-one stream)
	   ,(stream-case-more on-more stream))))))

(defun stream-cdr (stream)
  (stream-case stream
			   (nil)
			   ((a) nil)
			   ((a f) (funcall f))))

(defmacro mk-stream (car expr)
  `(cons ,car (lambda nil ,@expr)))



(eval-when-compile-also

(defmacro choice (a &optional f)
  `(cons ,a ,f))

(defun transform-binder (binder)
  (cond 
   ((symbolp binder) `(,binder ,binder))
   ((listp binder)
	(cond 
	 ((= 1 (length binder))
	  (let ((s (car binder)))
		`(,s ,s)))
	 ((= 2 (length binder))
	  binder)))
   (t (error "mk-stream-close needs binders which are symbols, lists of one symbol, or a let-like bind pair."))))

(defmacro mk-stream-close (binders car expr)
  `(lexical-let 
	   ,(mapcar #'transform-binder binders)
	 (cons ,car ,expr)))


(defun ->choice*binders (thing)
  (cond 
   ((symbolp thing) `((,thing ,thing)))
   ((listp thing) (mapcar #'transform-binder thing))))


(defmacro* choice* (a &optional f &key (with nil))
  `(lexical-let ,(->choice*binders with)
	 (cons ,a ,f)))
)
(example 
 (let ((stream (let ((q 10))
				 (choice* 33 (lambdac () (+ 1 q)) :with (q)))))
   (stream-cdr stream))
 )

(defun stream-zero ()
  nil)
(defvar stream-zero nil "Stream monad zero.")

(defun stream-unit (x)
  (stream-return x))

(defun stream-plus (stream f)
  (db-print stream)
  (db-print f)
  (stream-case stream
			   (funcall f)
			   ((a) (cons a f))
			   ((a f0) (cons a
								(lambda () (stream-plus (funcall f0) f)) :with (f f0)))))

(defun stream-plus^i (stream f)
  (stream-case stream
			   (funcall f) 
			   ((a) (cons a f))
			   ((a f0) (choice* a
								(stream-plus^i (funcall f) f0)))))

(defun stream-bind (stream g)
  (stream-case stream
			   (stream-zero)
			   ((a) (funcall g a))
			   ((a f) (stream-plus (funcall g a)
								   (lexical-let ((f f)
												 (g g)) 
									 (lambda ()
									   (stream-bind (funcall f) g)))))))

(defun stream-bind^i (stream g)
  (stream-case stream
			   (stream-zero)
			   ((a) (funcall g a))
			   ((a f) (stream-plus^i (funcall g a)
									 (lexical-let ((f f)
												   (g g)) 
									   (lambda ()
										 (stream-bind^i (funcall f) g)))))))

(defun %fail (s) (stream-zero))
(defvar %fail #'%fail)

(defun %succeed (s) (stream-unit s))
(defvar %succeed #'%succeed)

(defvar %s %succeed)
(defvar %u %fail)

(defun stream-return (x)
  (cons x nil))

(defvar monad-stream 
  (tbl! :m-bind #'stream-bind
		:m-return #'stream-return 
        :m-zero #'stream-zero))


(defvar monad-stream^i
  (tbl! :m-bind #'stream-bind^i
		:m-return #'stream-return 
        :m-zero #'stream-zero))

(eval-when-compile-also
(defmacro stream (&rest expressions)
  (cond
   ((not expressions)
	nil)
   ((= 1 (length expressions))
	`(choice ,(car expressions) nil))
   (t
	`(choice ,(car expressions)
			 (lambda ()
			   (stream ,@(cdr expressions))))))))

(defmacro streamc (&rest expressions)
  (cond
   ((not expressions)
	nil)
   ((= 1 (length expressions))
	`(choice ,(car expressions) nil))
   (t
	`(choice ,(car expressions)
			 (lambdac ()
			   (stream ,@(cdr expressions)))))))


(defun stream-cdr (stream)
  (stream-case stream
			   nil
			   ((a) nil)
			   ((a f) (funcall f))))

(defun stream-car (stream)
  (stream-case stream
			   nil
			   ((a) a)
			   ((a f) a)))

(defvar ones (choice 1 (lambda () ones))
  "An infinite stream of ones.")
(defvar zeros (choice 1 (lambda () zeros))
  "An infinite stream of zeros.")

(defun nums-from (n)
  "Return a stream of numbers from N to infinity."
  (choice* n (lambda () (nums-from (+ n 1))) :with n))

(defvar positive-integers 
  (nums-from 1) "An infinite stream of integers.")



(recur-defun* take-n (stream n &optional output)
  (if (= n 0) (reverse output)
	(stream-case 
	 stream
	 (reverse output)
	 ((a) (reverse (cons a output)))
	 ((a f)
	  (recur (stream-cdr stream) 
			 (- n 1)
			 (cons a output))))))


(defvar fibs (choice 1 (lambda () 
						 (choice 1 
								 (lambda ()
								   (mlet* monad-stream
											((a fibs)
											 (b (stream-cdr fibs)))
											(+ a b)))))))
(provide 'monad-stream)



