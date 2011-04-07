(require 'monads)

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
	   ,(stream-case-more on-more stream)))))

(defun stream-cdr (stream)
  (stream-case stream
			   (nil)
			   ((a) nil)
			   ((a f) (funcall f))))

(defmacro mk-stream (car expr)
  (cons ,car (lambda nil ,@expr)))

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

(defmacro choice (a f)
  `(cons ,a ,f))

(defun ->choice*binders (thing)
  (cond 
   ((symbolp thing) `((,thing ,thing)))
   ((listp thing) (mapcar #'transform-binder thing))))

(defmacro* choice* (a f &key (with nil))
  `(lexical-let ,(->choice*binders with)
	 (cons ,a ,f)))

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
  (stream-case stream
			   (funcall f)
			   ((a) (choice* a f :with (f)))
			   ((a f0) (choice* a
								(stream-plus (funcall f0) f) :with (f)))))

(defun stream-plus^i (stream f)
  (stream-case stream
			   (funcall f) 
			   ((a) (choice* a f :with (f)))
			   ((a f0) (choice* a
								(stream-plus^i (funcall f) f0) :with (f0)))))

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
  (cons x (lambda () nil)))

(defvar stream-monad 
  (tbl! :m-bind #'stream-bind
		:m-return #'stream-return 
        :m-zero #'stream-zero))


(defvar stream-monad^i
  (tbl! :m-bind #'stream-bind^i
		:m-return #'stream-return 
        :m-zero #'stream-zero))

(defmacro stream (&rest expressions)
  (cond
   ((not expressions)
	nil)
   ((= 1 (length expressions))
	`(choice ,(car expressions) nil))
   (t
	`(choice ,(car expressions)
			 (lambda ()
			   (stream ,@(cdr expressions)))))))

(cl-prettyprint (macroexpand-all '(stream 1 2 3 4)))
(cons 1
	  (function
	   (lambda nil
		 (cons 2
			   (function
				(lambda nil (cons 3 (function (lambda nil (cons 4 nil))))))))))


(domonad stream-monad 
		 [x (stream 1 2 3)
			y (stream 4 5 6)
			z (progn 
				(stream (+ x y)))]
		 z)s

( stream-bind (stream 1) (lambda (x) (stream (+ x 1))) )

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

(stream-case (stream 1 2 3)
			 nil
			 ((a) a)
			 ((a f) (funcall f)))

(stream-car (stream-cdr (stream-cdr (stream-cdr (stream 1 2 3 4)))))