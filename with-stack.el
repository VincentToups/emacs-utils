(require 'utils)
(require 'macro-utils)

(defun stack-atomp (item)
  (or (numberp item)
	  (stringp item)
	  (vectorp item)
	  (quotep item)))
(defun handle-stack-atom (item)
  (cond ((quotep item) `(push ,item *stack*))
		(t `(push ,item *stack*))))

(setf *stack-words* (tbl!))

(defun push-stack (x)
  (push x *stack*))
(defun pop-stack ()
  (pop *stack*))

(defun stack-wordp (item)
  (and (symbolp item)
	   ($ item in *stack-words*)))

(defmacro* defstackword (name &body body)
  (let ((actual-name (internf "stack-%s-" name)))
	`(progn 
	   (defun ,actual-name () ,@body)
	   (tbl! *stack-words* ',name (list ',actual-name nil)))))

(defmacro* defstackword-immediate (name &body body)
  (let ((actual-name (internf "stack-%s-" name)))
	`(progn 
	   (defun ,actual-name () ,@body)
	   (tbl! *stack-words* ',name (list ',actual-name t)))))

(defun stackword-immediatep (word)
  (cadr (tbl *stack-words* word)))

(defun handle-immediate-stackword (item)
  (let ((*stack* code)
		(*retain-stack* nil)
		(*old-stack* *stack*)) 
	(funcall (car (tbl *stack-words* item)))))

(defun handle-stack-word (item)
  (if (stackword-immediatep item) (handle-immediate-stackword item)
	`(,(car (tbl *stack-words* item)))))

(defun stack-emacs-callp (item)
  (let ((s (format "%s" item)))
	(and ($ ">" in s)
		 (let* ((parts (split-string s ">"))
				(n-stack-part (car parts)))
		   (or (eq n-stack-part "n")
			   (numberp (read n-stack-part)))))))

(defun gen-temp-syms (n)
  (loop for i from 1 to n collect 
		(gensym (format "stack-temp-%d" i))))

(defun pop-n (n where)
  "returns a list of the first n items WHERE, which are removed"
  (loop for i from 1 to n collect (pop where)))

(defun handle-emacs-call (item)
  (let* ((s (format "%s" item))
		 (parts (split-string s ">"))
		 (n (read (car parts)))
		 (sym (intern (join (cdr parts) ">")))
		 (list-sym (gensym "npop-"))
		 (temp-syms (gen-temp-syms n)))
	(cond ((and (symbolp n) (eq n 'n))
		   `(let ((,list-sym (pop-n *stack*)))
			  (appply #',sym (reverse ,list-sym))))
		  ((numberp n)
		   `(let ,(loop for s in temp-syms collect
						`(,s (pop *stack*)))
			  (push (,sym ,@(reverse temp-syms)) *stack*))))))

(defun handle-stack-symbol (item)
  (cond 
   ((or (eq item 't) (eq item t)) `(push t *stack*))
   ((stack-wordp item) (handle-stack-word item))
   ((stack-emacs-callp item) (handle-emacs-call item))
   (t (error (format "stack: Can't figure out how to compile %s." item)))))

(defmacro* with-stack- (&body code)
  `(progn
	 ,@(loop while code collect
			 (let ((item (car code)))
			   (setf code (cdr code)) 
			   (cond 
				((eq item nil)
				 `(push nil *stack*))
				((stack-atomp item)
				 (handle-stack-atom item))
				((symbolp item)
				 (handle-stack-symbol item)))))))

(defmacro* with-stack (&body code)
  `(let ((*stack* nil)
		 (*retain-stack* nil))
	 ,@(loop while code collect
			 (let ((item (car code)))
			   (setf code (cdr code)) 
			   (cond 
				((eq item nil)
				 `(push nil *stack*))
				((stack-atomp item)
				 (handle-stack-atom item))
				((symbolp item)
				 (handle-stack-symbol item)))))
	 (car *stack*)))

(defmacro* ||| (&body body)
  `(with-stack ,@body))
(defmacro* |||- (&body body)
  `(with-stack- ,@body))

(defun stack-at-least (n)
  (>= (length *stack*) n))

(defun retain-stack-at-least (n)
  (>= (length *retain-stack*) n))

(defmacro bivalent-stack-word (s)
  `(defstackword ,s 
	 (|||- ,(intern (format "2>%s" s)))))

(defstackword print (print (pop *stack*)))
(defstackword call
  (let ((qtn (pop *stack*)))
	(if (not (listp qtn)) (error "stack: call can't call something that isn't a list.")
	  (eval `(with-stack- ,@qtn)))))
(defstackword if 
  (if (stack-at-least 3)
	  (let* ((false-branch (pop *stack*))
			 (true-branch (pop *stack*))
			 (predicate (pop *stack*)))
		(if (and (listp false-branch)
				 (listp true-branch))
			(eval `(with-stack- ,@(if predicate true-branch false-branch)))
		  (error "stack: if requires two quotations on the stack.")))
	(error "stack: if needs at least three elements on the stack.")))

(defstackword loop 
  (let* ((qtn (pop *stack*))
		 (code (macroexpand-all `(with-stack- ,@qtn))))
	(eval code)
	(loop while (pop *stack*) do (eval code))))

(defstackword drop 
  (if *stack* (pop *stack*) (error "stack: stack underflow (from drop)")))
(defstackword swap 
  (if (stack-at-least 2)
	  (let ((top (pop *stack*))
			(new-top (pop *stack*)))
		(push top *stack*)
		(push new-top *stack*))
	(error "stack: swap needs at least two elements on the stack.")))
(defstackword curry 
  (if (stack-at-least 2)
	  (let ((qtn (pop *stack*))
			(item (pop *stack*)))
		(if (listp qtn)
			(push (cons item qtn) *stack*)
		  (error "stack: curry needs a list on the top of the stack.")))
	(error "stack: curry needs at least two arguments on the stack.")))
(defstackword compose 
  (if (stack-at-least 2)
	  (let ((qtn1 (pop *stack*))
			(qtn2 (pop *stack*)))
		(if (and (listp qtn1)
				 (listp qtn2))
			(push (append qtn2 qtn1) *stack*)
		  (error "stack: compose needs two lists on the stack.")))
	(error "stack: compose needs at least two arguments on the stack.")))
(defstackword dup 
  (push (car *stack*) *stack*))
(defstackword nip 
  (with-stack- swap drop))
(defstackword print-stack 
  (print (concat "stack:" (format "\n")
				 (loop 
				  with str = "" 
				  for object in (reverse *stack*) do
				  (setf str (concat str (format "- %s" object) (format "\n")))
				  finally (return str)))))

(defstackword dupd 
  (let ((top (pop *stack*)))
	(push (car *stack*) *stack*)
	(push top)))
(defstackword over 
  (push (elt *stack* 1) *stack*))
(defstackword pick
  (push (elt *stack* 2) *stack*))

(defstackword dip 
  (if (stack-at-least 1)
	  (let ((qtn (pop-stack))
			(hold (pop-stack)))
		(push-stack qtn)
		(|||- call)
		(push-stack hold))
	(error "stack: dip requires at least a quotation on the stack.")))

(defstackword swapd 
  (if (stack-at-least 3)
	  (let ((top (pop-stack)))
		(|||- swap)
		(push-stack top))
	(error "stack: swapd requires at least three items on the stack.")))

(defstackword rot 
  (if (stack-at-least 3)
	  (|||- swapd swap)
	(error "stack: rot requires at least three items on the stack.")))

(defstackword -rot 
  (if (stack-at-least 3)
	  (|||- swap swapd)
	(error "stack: -rot requires at least three items on the stack.")))

(defstackword tuck
  (if (stack-at-least 2)
	  (|||- swap over)))

(defstackword >r 
  (if (stack-at-least 1)
	  (push (pop-stack) *retain-stack*)
	(error "stack: >r requires at least something on the stack.")))

(defstackword r>
  (if (retain-stack-at-least 1)
	  (push-stack (pop *retain-stack*))
	(error "stack: r> requires at least one item on the retain stack.")))

(defstackword keep
  (|||- over '(call) dip))

(defstackword bi 
  (|||- '(keep) dip call))

(defstackword + 
  (|||- 2>+))
(defstackword -
  (|||- 2>-))
(defstackword *
  (|||- 2>*))
(defstackword /
  (|||- 2>/))
(defstackword and 
  (|||- 2>and))
(defstackword or
  (|||-> 2>or))

(defstackword stack 
  (push-stack *stack*))

(defstackword retain-stack
  (push-stack *retain-stack*))

(defstackword bi*
  (|||- '(dip) dip call))

(defstackword bi@
  (|||- dup bi*))