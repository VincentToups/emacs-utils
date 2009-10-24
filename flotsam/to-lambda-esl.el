(require 'utils)
(require 'defn)

(setf *esl-stack* nil)
(setf *esl-return-stack*)

(defn esl-push [item]
										;(print *esl-stack*)
  (push item *esl-stack*))
(defn esl-pop []
										;(print *esl-stack*)
  (if ($ 0 = (length *esl-stack*))
	  (error "ESL: stack underflow %s" *esl-stack*))
  (pop *esl-stack*))
(defn esl-dup []
  (esl-push (car *esl-stack*)))
(defn esl-swap []
  (let ((a (esl-pop))
		(b (esl-pop)))
	(esl-push a)
	(esl-push b)))


(defun esl-mangle (name)
  (intern (format "__els-word__%s" name)))

(defun esl-unmangle (name)
  (intern (substring (format "%s" name) 0 12)))

(defmacro* def-esl-word (name &body body)
  `(defun ,(esl-mangle name) ()
	 ,@body))

(def-esl-word + (esl-push (+ (esl-pop) (esl-pop))))
(def-esl-word print (print (esl-pop)))
(def-esl-word drop-all (setf *esl-stack* nil))
(def-esl-word call (funcall (esl-pop)))
(def-esl-word over (esl-push (cadr *esl-stack*)))
(def-esl-word dip (let ((qtn (esl-pop))
						(hold (esl-pop)))
					(esl-push qtn)
					(esl-eval-compiled '(__els-word__call))
					(esl-push hold)))
(def-esl-word curry (let ((qtn (esl-pop))
						  (it (esl-pop)))
					  (esl-push `((lambda () (esl-push ,it))
								  ,@qtn))))
(def-esl-word = (esl-push (= (esl-pop) (esl-pop))))
(def-esl-word not (esl-push (not (esl-pop))))


(defn_ compile-esl [esl-code]
  (eval `(function (lambda ()
					 ,@(loop for part in esl-code collect
							 (cond
							  ((listp part)
							   `(esl-push (quote ,(compile-esl part))))
							  ((symbolp part)
							   (progn 
								 (if (not (functionp (esl-mangle part)))
									 (print (format "warning, kind find word %s" part)))
								 (list (esl-mangle part))))
							((or 
							  (numberp part)
							  (stringp part))
							 `(esl-push ,part))))))))

(funcall (compile-esl '( drop-all 1 1 + print (1 1 +))))
(defun bottomless () (bottomless))
(bottomless)


(defun esl-eval-compiled (code)
  (funcall code))

(esl-eval-compiled (compile-esl '(drop-all 10 20 + print (1 2 + print) call)))
(esl-eval-compiled (compile-esl '( drop-all 1 4 ( + ) curry call print)))

*esl-stack*

(setf x 2000)
(let ((x 20))
  (funcall (function (eval `(lambda (y) ,x))) 20))
					 
(funcall (eval `(function (lambda (x) ,10))) 100)
