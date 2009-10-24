(require 'utils)
(require 'defn)

(setf *esl-stack* nil)
(setf *esl-return-stack*)
(setf *esl-code-stack*)

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
  (intern (format "__esl-word__%s" name)))

(defun esl-unmangle (name)
  (intern (substring (format "%s" name) 0 12)))

(defmacro* def-esl-word (name &body body)
  `(defun ,(esl-mangle name) ()
	 ,@body))
(defmacro* def-esl-binop (op)
  `(def-esl-word ,op (esl-swap) (esl-push (,op (esl-pop) (esl-pop)))))

(defvar *esl-parsing-words* (tbl!))

(defmacro* def-esl-parsing-word (name &body body)
  (tbl! *esl-parsing-words* (esl-mangle name) t)
  `(def-esl-word ,name ,@body))

(defun parsing-word? (mangled-name)
  (member mangled-name (keyshash *esl-parsing-words*)))


(def-esl-word + (esl-push (+ (esl-pop) (esl-pop))))
(def-esl-word - (esl-swap) (esl-push (- (esl-pop) (esl-pop))))
(loop for op in '( * / mod cons < > <= >=) do
	  (eval `(def-esl-binop ,op)))
(def-esl-word and (esl-swap) (let ((a (esl-pop)) (b (esl-pop))) (and a b)))
(def-esl-word or (esl-swap) (let ((a (esl-pop)) (b (esl-pop))) (or a b)))
(def-esl-word print (print (esl-pop)))
(def-esl-word drop-all (setf *esl-stack* nil))

(def-esl-word call 
  (let ((qtn (esl-pop)))
	(if (functionp qtn) (funcall qtn)
	  (progn
		(if *esl-code-stack*
			(push *esl-code-stack* *esl-return-stack*))
		(setf *esl-code-stack* qtn)))))

(def-esl-word over (esl-push (cadr *esl-stack*)))
(def-esl-word dip (let ((qtn (esl-pop))
						(hold (esl-pop)))
					(esl-push qtn)
					(esl-eval-compiled '(__esl-word__call))
					(esl-push hold)))
(def-esl-word curry (let ((qtn (esl-pop))
						  (it (esl-pop)))
					  (esl-push `((lambda () (esl-push ',it))
								  ,@qtn))))
(def-esl-word = (esl-push (= (esl-pop) (esl-pop))))
(def-esl-word not (esl-push (not (esl-pop))))
(def-esl-word if (let ((fb (esl-pop))
					   (tb (esl-pop)) 
					   (predval (esl-pop)))
				   (if predval 
					   (esl-push tb)
					 (esl-push fb))
				   (push (esl-mangle 'call) *esl-code-stack* )
				   ))

(def-esl-word t (esl-push t))
(def-esl-word f (esl-push nil))
(def-esl-word quot>word (let ((name (esl-pop))
							  (qtn (esl-pop)))
						  (eval `(def-esl-word ,name 
								   (push ',qtn *esl-return-stack*)))))
(def-esl-word dup (esl-dup))
(def-esl-word drop (esl-pop))
(def-esl-word call-emacs-push 
  (let ((emacs-fn (esl-pop))
		(nargs (esl-pop)))
	(esl-push (apply emacs-fn (reverse (loop for i from 1 to nargs collect (esl-pop)))))) )

(def-esl-parsing-word : 
  (let ((split (split-list-drop *esl-stack*
								(lambda (v) (eq v 'end-word:)))))
	(if split
		(dlet_ [[qtn rest] split]
		  (setf *esl-stack* rest)
		  (esl-push 'quot>word)
		  (esl-push (list 'quote (car qtn)))
		  (esl-push (cdr qtn)))
	  (error "Could not find terminating end-word: for %s" (car *esl-stack*)))))

(defn_ quotedp [it]
  (and (listp it)
	   (eq 'quote (car it))))

(defn_ esl-hard-compile [esl-code]
  (eval `(function (lambda nil ,@(let ((*esl-stack* esl-code))
	(loop with output = nil 
		  while *esl-stack* 
		  do
		  (let ((part (esl-pop)))
			(cond
			 
			 ((listp part)
			  (push (if (quotedp part)
						`(esl-push ,part)
					  `(esl-push (quote ,(esl-compile part)))) output))
			 ((symbolp part)
			  (cond ((parsing-word? (esl-mangle part))
					 (funcall (esl-mangle part)))
					(t
					 (push (list (esl-mangle part)) output))))
			 ((or 
			   (numberp part)
			   (stringp part))
			  (push `(esl-push ,part) output))))
		  finally (return (reverse output))))))))

(defn_ esl-compile [esl-code]
  (let ((*esl-stack* esl-code))
	(loop with output = nil 
		  while *esl-stack* 
		  do
		  (let ((part (esl-pop)))
			(cond

			 ((listp part)
			  (push (if (quotedp part)
						(eval `(function (lambda () (esl-push ,part))))
					  (eval `(function (lambda () (esl-push (quote ,(esl-compile part))))))) output))
			 ((symbolp part)
			  (cond ((parsing-word? (esl-mangle part))
					 (funcall (esl-mangle part)))
					(t
					 (push (esl-mangle part) output))))
			 ((or 
			   (numberp part)
			   (stringp part))
			  (push (eval `(function (lambda () (esl-push ,part)))) output))))
		  finally (return (reverse output)))))

(defn_ body-of [[l arg & body]]
  body)

(defn_ esl-soft->hard-compile [qtn]
  (loop for el in qtn 
		collect
		(cond ((functionp el)
			   (body-of el))
			  ((symbolp el) 
			   (list el)))))
				  

(defun esl-next-part ()
  (cond (*esl-code-stack*
		 (pop *esl-code-stack*))
		(*esl-return-stack* 
		 (setf *esl-code-stack* (pop *esl-return-stack*))
		 (pop *esl-code-stack*))
		(t 
		 (error "Tried to get next part, but both the code stack and the return stack are empty - this should not happen."))))
  

(defun esl-eval-compiled (ccode)
  (let ((*esl-code-stack* ccode))
	(loop while 
		  (or *esl-return-stack* 
			  *esl-code-stack*)
		  do
		  (let ((part (esl-next-part)))
			(funcall part)))))
  
(defmacro* esl-do (&body body)
  `(esl-eval-compiled (esl-compile ',body)))

(defmacro* esl-dop (&body body)
  `(progn (esl-do ,@body)
		  (esl-print-state)
		  nil))
  
(defun esl-print-state ()
  (let ((stack (loop with o = "" for s in (reverse *esl-stack*) do
					 (setf o (concat o (format "\n %s" s)))
					 finally (return o))))
	(print (format "------------------\nesl-stack:\n- %s\n -----------------" stack))))

(dont-do
 (esl-dop drop-all 1 (1 +) curry call))

(esl-dop drop-all
  : when nil if end-word:)

(esl-dop drop-all 110 t ( 1 + ) when )

(esl-dop : keep over (call) dip end-word:
		 : loop (call) keep (loop) curry when end-word:
		 : incr 1 + end-word:) 

(esl-compile '(nil))
(esl-do drop-all 5 5 * print )




