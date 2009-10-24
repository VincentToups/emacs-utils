(require 'utils)
(require 'defn)

(setf *esl-stack* nil)
(setf *esl-return-stack*)
(setf *esl-code-stack*)
(setf *esl-retain-stack*)

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


(loop for op in '( = - * / mod cons < > <= >= + - eq string=) do
	  (eval `(def-esl-binop ,op)))
(def-esl-word and (esl-swap) (esl-push (let ((a (esl-pop)) (b (esl-pop))) (and a b))))
(def-esl-word or (esl-swap) (esl-push (let ((a (esl-pop)) (b (esl-pop))) (or a b))))
(def-esl-word print (print (esl-pop)))
(def-esl-word drop-all (setf *esl-stack* nil))
(def-esl-word swap (esl-swap))
(def-esl-word rot (let ((top (esl-pop))
						(mid (esl-pop))
						(bot (esl-pop)))
					(esl-push mid)
					(esl-push top)
					(esl-push bot)
					))

(def-esl-word error (error (concat "ESL: " (esl-pop))))

(setf *esl-called-from-hard-compiled* nil)

(def-esl-word call
  (let* ((qtn (esl-pop))
		 (qtn-hard (functionp qtn))
		 (caller-hard *esl-called-from-hard-compiled*))
	(cond
	 (qtn-hard
	  (funcall qtn))
	 (caller-hard 
	  (esl-eval-compiled qtn))
	 ((not caller-hard)
	  (if *esl-code-stack* (push *esl-code-stack* *esl-return-stack*))
	  (setq *esl-code-stack* qtn)))))


(def-esl-word over (esl-push (cadr *esl-stack*)))

(def-esl-word curry 
  (let ((qtn (esl-pop))
		(it (esl-pop)))
	(esl-push (if (functionp qtn)
				  (list (eval `(function (lambda () (esl-push ',it))))
						qtn)
				(cons (eval `(function (lambda () (esl-push ',it))))
					  qtn)))))


(defun esl-qtn? (it)
  (or (listp it) (functionp it)))
(def-esl-word compose 
  (let ((q2 (esl-pop))
		(q1 (esl-pop)))
	(if (not (and (esl-qtn? q1)
				  (esl-qtn? q2)))
		(error "ESL:Trying to compose non-quotations."))
	(esl-push (cond 
			   ((and (functionp q1)
					 (functionp q2))
				(eval `(function (lambda () (funcall ,q1)
								   (funcall ,q2)))))
			   ((and (not (functionp q1))
					 (not (functionp q2)))
				(append q1 q2))
			   ((and (functionp q1)
					 (not (functionp q2)))
				(cons q1 q2))
			   ((and (not (functionp q1))
					 (functionp q2))
				(append q1 (list q2)))))))



(def-esl-word = (esl-push (= (esl-pop) (esl-pop))))
(def-esl-word not (esl-push (not (esl-pop))))
(def-esl-word if (let ((fb (esl-pop))
					   (tb (esl-pop)) 
					   (predval (esl-pop)))
				   (if predval 
					   (esl-push tb)
					 (esl-push fb))
				   (__esl-word__call)
				   ))

(def-esl-word t (esl-push t))
(def-esl-word f (esl-push nil))
(def-esl-word quot>word (let ((name (esl-pop))
							  (qtn (esl-pop)))
						  (eval `(def-esl-word ,name
								   (esl-push ',qtn)
								   (let ((*esl-called-from-hard-compiled* nil))
									 (__esl-word__call))))))

(def-esl-word dup (esl-dup))
(def-esl-word drop (esl-pop))

(def-esl-word retain> (push (esl-pop) *esl-retain-stack*))
(def-esl-word <retain (esl-push (pop *esl-retain-stack*)))
(def-esl-word push-retain (push (esl-pop) *esl-retain-stack*))
(def-esl-word pop-retain (esl-push (pop *esl-retain-stack*)))

(def-esl-word retain-dup (push (car *esl-retain-stack*) *esl-retain-stack*))
(def-esl-word retain-swap (let ((a (pop *esl-retain-stack*))
								(b (pop *esl-retain-stack*)))
							(push a *esl-retain-stack*)
							(push b *esl-retain-stack*)))

(def-esl-word call-emacs-push 
  (let* ((emacs-fn (esl-pop))
		 (nargs (esl-pop))
		 (args (reverse (loop for i from 1 to nargs collect (esl-pop)))))
	
	(let ((result (apply emacs-fn args)))
	  (esl-push result))))

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

(def-esl-word mark-as-parsing 
  (let ((name (esl-pop)))
	(if (symbolp name)
		(tbl! *esl-parsing-words* (esl-mangle name) t)
	  (error "You tried to mark %s as  parsing word, but its not a symbol" name))))

(def-esl-parsing-word PARSING:
  (let ((split (split-list-drop *esl-stack*
								(lambda (v) (eq v 'end-word:)))))
	(if split
		(dlet_ [[qtn rest] split]
		  (setf *esl-stack* rest)
		  (esl-push 'mark-as-parsing)
		  (esl-push (list 'quote (car qtn)))
		  (esl-push 'quot>word)
		  (esl-push (list 'quote (car qtn)))
		  (esl-push (cdr qtn)))
	  (error "Could not find terminating end-word: for %s" (car *esl-stack*)))))

(def-esl-parsing-word hard-compile-quotation 
  (let ((qtn (esl-pop)))
	(esl-push (esl-soft->hard-compile qtn))))

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
  (let ((*esl-stack* esl-code)
		(*esl-code-stack* nil)
		(*esl-retain-stack* nil)
		(*esl-return-stack* nil))
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
						  (esl-eval-compiled (list (esl-mangle part))))
						 (t
						  (push (esl-mangle part) output))))
				  ((or 
					(numberp part)
			   (stringp part))
			  (push part output))))
		  finally (return (reverse output)))))

(defn_ body-of [[l arg & body]]
  body)

(defn_ esl-soft->hard-compile [qtn]
  (eval `(function (lambda () ,@(loop for el in qtn 
									  collect
									  (cond 
									   ((symbolp el) 
										(list el))
									   ((functionp el)
										`(progn ,@(body-of el))
										)))))))

(defun esl-next-part ()
  (cond (*esl-code-stack*
		 (pop *esl-code-stack*))
		(*esl-return-stack* 
		 (setf *esl-code-stack* (pop *esl-return-stack*))
		 (pop *esl-code-stack*))
		(t 
		 (error "Tried to get next part, but both the code stack and the return stack are empty - this should not happen."))))


(defun esl-eval-compiled (ccode)
  (if (functionp ccode)
	  (let ((*esl-called-from-hard-compiled* t))
		(functionp ccode))
	(let ((*esl-code-stack* ccode)
		  (*esl-called-from-hard-compiled* nil))
	  (loop while 
			(or *esl-return-stack* 
				*esl-code-stack*)
			do
			(let ((part (esl-next-part)))
			  (cond ((or (stringp part) (numberp part))
					 (esl-push part))
					((functionp part)
					 (funcall part))))))))

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
	(print (format "------------------\nesl-stack:\n- %s\n------------------" stack))))

(defun esl-eval-region (s e)
  (interactive "r")
  (let* ((fixed (concat "(" (buffer-substring s e) ")")))
	(esl-eval-compiled (esl-compile (read fixed)))
	(esl-print-state)))


(require 'esl-mode)








  

