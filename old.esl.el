(require 'cl)
(require 'utils)
(require 'defn)

(setf *esl-stack* nil)
(setf *esl-return-stack*)
(setf *esl-core-vocab* (tbl!))
(setf *vocab-stack* (list *esl-core-vocab*))
(setf *retain-stack* nil)

(defn_ esl-next-word* [ [stack 
						 return-stack
						 retain-stack 
						 vocab-stack] ]
  (loop while (and (eq nil (car return-stack))
				   return-stack) do
				   (pop return-stack))
  (dlet_ [[cinst & rinst] return-stack
          [word  & rest] cinst]
	(list word (list stack
					 (cons rest rinst)
					 retain-stack
					 vocab-stack))))

(esl-next-word (list '() () () () '(  (a b c) (d e f) ) () ()))

(defn_ esl-next-word [esl-input-stream]
  (when esl-input-stream
	(loop while (and (eq nil (car esl-input-stream))
					 esl-input-stream)
		  do (pop esl-input-stream))
	(dlet_ [[cinst & rinst] esl-input-stream
			[word  & rest] cinst]
	  (list word (if rest (cons rest rinst) rinst)))))

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

(dont-do
 (esl-push 10)
 (esl-pop)
 *esl-stack*)

(defn lookup-word [w vocab-stack]
  (cond ((eq nil vocab-stack) (error (format "could not find word %s" w)))
		((member w (keyshash (car vocab-stack))) (tbl (car vocab-stack) w))
		(t (lookup-word w (cdr vocab-stack)))))

(defn_ quotedp [it]
  (and (listp it)
	   (eq (car it) 'quote)))

(defun esl-print-state ()
  (let ((stack (loop with o = "" for s in (reverse *esl-stack*) do
					 (setf o (concat o (format "\n %s" s)))
					 finally (return o))))
	(print (format "------------------\nfuture: %s\nstack:\n%s\n------------------" esl-input-stream stack))))

(defun esl-interpret (esl-input-stream)
  (cond
   ((eq nil esl-input-stream)  nil)
   ((listp esl-input-stream)
	(loop while esl-input-stream do
		  (let-seq (it rest) (esl-next-word esl-input-stream)
				   (setf esl-input-stream rest)
				   (cond
					((eq nil it) (esl-push nil)) 
					((quotedp it)  (esl-push (cadr it)))
					((numberp it)  (esl-push it))
					((stringp it)  (esl-push it))
					((keywordp it)  (esl-push it))
					((symbolp it)
					 (let ((word (lookup-word it *vocab-stack*)))
					   (funcall word)))))))))

(defmacro* esl (&body body) `(esl-interpret (list '(,@body))))
(defmacro* eslp (&body body) `(esl-interpret (list '(,@body print-stack))))

(defn esl-add-core-fn [nm code]
  (tbl! *esl-core-vocab* nm code))

(defn esl-add-to-current-vocabulary [nm code]
  (tbl! (car *vocab-stack*) nm code))

(esl-add-core-fn 'current-future (lambda () esl-input-stream))
(esl-add-core-fn 'print (lambda () (print (esl-pop))))
(esl-add-core-fn '+ (lambda () (esl-push (+ (esl-pop) (esl-pop)))))
(esl-add-core-fn '- (lambda () (esl-push (progn (esl-swap) (- (esl-pop) (esl-pop))))))
(esl-add-core-fn '* (lambda () (esl-push (* (esl-pop) (esl-pop)))))
(esl-add-core-fn '/ (lambda () (esl-push (progn (esl-swap) (/ (esl-pop) (esl-pop))))))
(esl-add-core-fn 'concat (lambda () (esl-push (progn (esl-swap) (concat (esl-pop) (esl-pop))))))
(esl-add-core-fn 'drop (lambda () (esl-pop)))
(esl-add-core-fn 'call-emacs-fn (lambda ()
								  (esl-push (let* ((fname (esl-pop))
												   (n-arg (esl-pop))
												   (args
													(reverse (loop for i from 1 to n-arg 
																   collect (esl-pop)))))
											  (apply fname args)))))
(esl-add-core-fn 'drop-all (lambda () (setf *esl-stack* nil)))


(defn_ grab-word-body []
  (loop while (and esl-input-stream (not (eq (car esl-input-stream) 'end-word:))) collect (pop esl-input-stream) into output
		finally (pop esl-input-stream) (return output)))

(setf current-word: 'no-current-word)
(defun grab-word-body ()
  (let* ((current-code-block (car esl-input-stream))
		 (rest (cdr esl-input-stream))
		 (split (split-list-drop current-code-block
								 (fn_ [x] (eq x 'end-word:)))))
	(if split
		(let-seq (word-body rest-of-block) split
				 (setf esl-input-stream (cons rest-of-block rest))
				 word-body)
	  (error "Unable to find terminating end-word: for word: %s" current-word:))))

(dont-do 
 (let ((esl-input-stream '( (a b c d end-word:) (e f g))))
   (print esl-input-stream)
   (grab-word-body)))

(dont-do (grab-word-body '(a b c d end-word: e f g)))

(esl-add-core-fn 'word: (lambda ()
						  (let-seq (name rest) (esl-next-word esl-input-stream)
								   (setf esl-input-stream rest)
								   (lexical-let
									   ((body (let ((current-word: name)) (grab-word-body))))
									 (esl-add-to-current-vocabulary name (lambda () (push body esl-input-stream)))))))

(esl-add-core-fn 'interactive-word:
				 (lambda ()
				   (let ((name (pop esl-input-stream))
						 (interactive-desc (pop esl-input-stream)))
					 (lexical-let ((body (grab-word-body)))
					   (esl-add-to-current-vocabulary name (lambda () (setf esl-input-stream (append body esl-input-stream)))))
					 (eval `(defun ,name (&rest args) (interactive ,interactive-desc)
							  (esl-interpret (append args (list ',name))))))))

(esl-add-core-fn 'call 
				 (lambda () 
				   (let ((code (esl-pop)))
					 (push code esl-input-stream))))

(dont-do
 (setf *esl-stack* nil)
 *esl-stack*
 (esl-interpret '((1 1 '(+) call)))
 (eslp drop-all f)
  (eslp drop-all 1 1 f '('(+)) '('(-)) if call print)
  )

 (esl-add-core-fn 'if
				  (lambda ()
					(let ((fbranch (esl-pop))
						  (tbranch (esl-pop))
						  (pred (esl-pop)))					 
					  (if pred (push tbranch esl-input-stream)
						(push fbranch esl-input-stream)))))

 (esl-add-core-fn 't (lambda () (esl-push t)))
 (esl-add-core-fn 'f (lambda () (esl-push nil)))
 (esl-add-core-fn 'dip (lambda () (let ((quot (esl-pop))
										(hold (esl-pop)))
									(esl-push quot)
									(esl call)
									(esl-push hold))))
 (esl-add-core-fn '>r (lambda () (push (esl-pop) *retain-stack*)))
 (esl-add-core-fn '<r (lambda () (esl-push (pop *retain-stack*))))
 (esl-add-core-fn 'rdup (lambda () (push (car *retain-stack*) *retain-stack*)))
 (esl-add-core-fn 'print-stack (lambda ()
								 (esl-print-state)))

 (esl-add-core-fn 'swap (lambda () (esl-swap)))
 (esl-add-core-fn 'dup (lambda () (esl-dup)))
 (esl-add-core-fn 'over (lambda () (esl-push (cadr *esl-stack*))))
 (esl-add-core-fn 'curry (lambda () (let ((qu (esl-pop))
										  (it (esl-pop)))
									  (esl-push (cons `(quote ,it) qu)))))
(esl-add-core-fn '= (lambda () (esl-push (= (esl-pop) (esl-pop)))))
(esl-add-core-fn '< (lambda () (esl-push (progn (esl-swap) (< (esl-pop) (esl-pop))))))
(esl-add-core-fn '> (lambda () (esl-push (progn (esl-swap) (> (esl-pop) (esl-pop))))))
(esl-add-core-fn '<= (lambda () (esl-push (progn (esl-swap) (<= (esl-pop) (esl-pop))))))
(esl-add-core-fn '>= (lambda () (esl-push (progn (esl-swap) (>= (esl-pop) (esl-pop))))))
(esl-add-core-fn 'not (lambda () (esl-push (not (esl-pop)))))
(esl-add-core-fn 'loop (lambda () (let ((qutn (car *esl-stack*)))
									(esl call)
									(loop while (esl-pop) do
										  (esl-push qutn)
										  (esl call)))))

(esl word: keep over '(call) dip end-word:)
(esl word: when '() if end-word:)
(esl word: loop '(call) keep '(loop) curry when end-word:)
(eslp word: incr 1 + end-word:)
(eslp drop-all 1 '(incr dup 100 = not) loop print)
(eslp 1 2 < print)
(eslp)




