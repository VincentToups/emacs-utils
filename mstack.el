(require 'monads)
(require 'utils)

(defun get-options (options) (cdr options))
(defun options? (mb-options) 
  (and (listp mb-options)
	   (eq (car mb-options) 'Options)))
(defun list->options (lst)
  (cons 'Options lst))


(defun options-bind (v f)
  (let ((options (get-options v)))
	(list->options (mapcat (comp #'get-options f) options))))

(defvar options-monad
  (tbl!
   :m-bind 
   #'options-bind
   :m-return 
   (lex-lambda (v)
			   (list->options (list v))))
  "Options monad - just window dressing on the list monad.")


(defun fpush (x stack) (cons x stack))
(defun fdrop (stack) (cdr stack))

(defun mfpush (mitems mstack)
  (funcall (m-lift-into2 #'fpush options-monad) mitems mstack))
(defun mfdrop (mstack)
  (funcall (m-lift-into1 #'fdrop options-monad) mstack))

(mfdrop (mfpush '(Options a b c) '(Options () (a) (a a))))

(domonad options-monad 
		 [x '(Options 1 2 3)
			y '(Options 4 5 6)]
		 (list x y))

(get-options '(Options a b c))