Lets use these functions to write some theoretical `social networking`
functions.  

    (defvar *no-one* nil)
    (defvar friends-db (alist>>
      :leo (list :ted :steve :fred :lea :jane)
      :ted (list :leo :steve :lea)
      :steve (list :leo)
      :fred (list :leo :lea :jane)
      :lea (:leo 
    (defun friends-of (person)



(require 'monad-stream)

(stream-plus^i (streamc 1 2 3) (lambdac () (stream nil)))

(stream-plus-rec (streamc 1 2 3) (lambdac () (stream nil)))

(reverse '(a b c . d))

(let ((x '(1)))
  (setf (cdr x) 10)
  x)

(defun improper-suffix (list item &optional past)
  (let* ((cp (copy-list list))
		 (cp-cdr (cdr cp)))
	(loop until ( = (length cp-cdr) 1) do
		  (setq cp-cdr (cdr cp-cdr)))
	(setf (cdr cp-cdr) item)
	cp))


(stream-bind ones (lambda (x) (stream (+ x 1))))

(with-monad monad-stream
			(stream-cdr (stream-cdr 
						 (mlet* monad-stream ((x (stream 1 2 3))
											  (y (stream 4 5 6)))
								(+ x y)))))

(stream-cdr (stream-cdr (stream-cdr 
			 (mlet*_ monad-stream ((x (stream 1 2 3))
								   (y (stream 4 5 6)))
					 (stream-return (+ x y))))))

(take-n (mlet*_ monad-stream ((x (streamc 1 2 3))
								   (y (streamc 4 5 6)))
					 (cons (+ x y) nil)) 3)

(fmakunbound 'choice)

(defmacro choice (a &optional f)
  `(cons ,a ,f))

(defun choice (a &optional f)
  (cons a f))

(with-monad monad-stream
			(m-return (+ 1 2)))

(cons (+ 1 1) nil)
(choice (+ 1 1) nil)

(choice 'x nil)

(defun add-streams (stream1 stream2)
  (stream-case stream1
			   nil
			   ((a1)
				(stream-case stream2
							 nil
							 ((a2) (choice (+ a1 a2) nil))
							 ((a2 f2)
							  (choice 

							   (defvar fibs (choice 1 (lambda () (choice 1 (lambda () 
											  (