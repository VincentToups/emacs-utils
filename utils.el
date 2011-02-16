;;; utils.el 
;;; Some of Vincent's utils.

(require 'cl)

(defvar lb "
" "A linebreak.")

(setq true t)

(defun list->vector (lst)
  "Convert a list to a vector."
  (coerce lst 'vector))

(defun vector->list (vec)
  "Convert a vector to a list."
  (coerce vec 'list))

(defun get-current-line-substring ()
  "Returns the current line from currently active buffer."
  (buffer-substring (get-beginning-of-line) (get-end-of-line)))

(defun last-line? ()
  "Returns true when the point is on the last line of the buffer."
  (save-excursion
	(eq (line-number-at-pos)
		(progn (forward-line 1) (line-number-at-pos)))))

(defun with-point-at-lines-start (fun)
  "Evaluates the function FUN with the point at the beginning of each line.  FUN is passed the line number."
  (save-excursion 
	(goto-char (point-min))
	(loop until (last-line?) do
		  (beginning-of-line)
		  (funcall fun (line-number-at-pos))		 
		  (forward-line 1))))

(defun line-numbers-of-region (startp endp)
  "Returns the line numbers of a region."
  (loop for i from (line-number-at-pos startp) to (line-number-at-pos endp) collect i))

(defun betweeni? (it bel ab)
  "Inclusive between-ness predicate.  True if BEL <= IT <= AB"
  (and (>= it bel) (<= it ab)))

(defun current-line-in-region? (startp endp)
  "Returns true when the current line is in the region STARTP ENDP."
  (betweeni? (line-number-at-pos) (line-number-at-pos startp) (line-number-at-pos endp)))

(defun comment-region-inv (startp endp)
  "Comments a region."
  (interactive "r")
  (with-point-at-lines-start
   (lambda (ln)
	 (if (not (current-line-in-region? startp endp))
		 (insert comment-start)))))

(defun uncomment-region-inv (startp endp)
  "Uncomments a region."
  (interactive "r")
  (with-point-at-lines-start
   (lambda (ln)
	 (if (not (current-line-in-region? startp endp))
		 (kill-forward-chars 1)))))

(defun jlet-bindings->let-bindings (bindings)
  "Converts clojure unpaired vector binding expressions to regular let binding expressions."
  (if (not (mod (length bindings) 2)) (error "jlet binding form needs an even number of items.")
	(let ((let-bindings '()))
	  (loop for i from 0 below (- (length bindings) 1) by 2 do
			(push (list (elt bindings i) (elt bindings (+ i 1))) let-bindings))
	  (reverse let-bindings))))

(defmacro jlet (bindings &rest body)
  "Clojure-ish let statement, (let [n1 v1 n2 v2] ...).  Does not support destructuring, but see defn.el."
  `(let* ,(jlet-bindings->let-bindings bindings) ,@body))

(defmacro llet (&rest args)
  "Short form LEXICAL-LET macro."
  `(lexical-let ,@args))

(defmacro jllet (bindings &rest body)
  "Short form paired vector binding lexical let."
  `(lexical-let* ,(jlet-bindings->let-bindings bindings) ,@body))

(defun in-string (sub str)
  "Returns true when SUB is in STR.  SUB can be a regular expression."
  (let ((new-string (replace-regexp-in-string sub "" str)))
	(not (string= new-string str))))

(defun* in (item lst &optional (pred #'eq))
  "returns true if ITEM is in LST where LST might be a hash table.  PRED determines equality, defaults to eq.  If item and lst are strings, then returns true if item is a substring of lst."
  (cond ((and 
		  (stringp item)
		  (stringp lst))
		 (in-string item lst))
		((hash? lst)
		 (in item (keyshash lst) pred))
		(t
		 (let* ((found nil))
		   (loop while (and lst (not found)) do
				 (if (funcall pred (car lst) item)
					 (progn (setq found t))
				   (progn
					 (setq lst (cdr lst)))))
		   found))))

(defun* unique (lst &optional (pred #'eq))
  "Returns a new list with only the unique elements in LST under PRED."
  (reverse (foldl
			(lambda (it ac)
			  (if (in it ac pred) ac
				(cons it ac)))
			'()
			lst)))

(defun insertf (&rest args)
  "Insert with string format string semantics on input."
  (insert (apply #'format  args)))

(defun make-keyword (name)
  "Creates a keyword from a string, doesn't need ':'"
  (intern (format ":%s" name)))

(defun bang (sym)
  "Return a new symbol with value <SYM>!"
  (intern (format "%s!" sym)))
(defun s-cat (sym1 sym2)
  "Concatenates two symbols."
  (intern (format "%s-%s" sym1 sym2)))
(defun ques (sym)
  "Returns a symbol with an appended question mark."
  (intern (format "%s?" sym)))

(defun suffix (lst x)
  "Appends X to the end of LST."
  (append lst (list x)))

(defmacro defstruquine (name &rest slots)
  "Defines a set of functions to access a series of slots in a list by name.  (defstruquine test a b c) defines test!, test?, test-a, test-b, and test-c which create, test-for and access the new struct."
  (let* ((n-fields (length slots))
		 (i 1)
		 (out `(progn
				 (defun ,(bang name) ,slots
				   (list ',(bang name) ,@slots)) 
				 (defun ,(ques name) (item)
				   (eq (car item) ',(bang name))))))
	(loop for slot in slots do
		  (setf out 
				(append out
						(list `(defun ,(s-cat name slot) (item) (elt item ,i)))))
		  (setf i (+ i 1)))
	(append out (list nil))))

(defun between-inc (low high val)
  "Inclusive between predicate."
  (and (>= val low)
	   (<= val high)))

(defun between (low high val)
  "Not inclusive between predicate."
  (and (> val low)
	   (< val high)))

(defun foldl (fn init list)
  "Folds a function over list with an accumulator initialised to init.  Left version."
  (let ((output init))
	(loop for item in list do
		  (setq output (funcall fn item output)))
	output))

(defun all-but-last (lst)
  "returns all but the last element of lst."
  (reverse (cdr (reverse lst))))

(defun foldla (fn init-and-list)
  "Uses the initial element of init-and-list as the initial value for foldl."
  (let ((lst (last init-and-list))
		(inits (all-but-last init-and-list)))
	(loop for item in lst do
		  (setq inits (apply fn (cons item inits))))
	inits))

(defun sum (&rest args)
  "Adds up the elements of the arg list.  Identical to +, apparently."
  (foldl #'+ 0 args))

(defun mean (&rest args)
  "Calculates the mean value of args."
  (/ (apply #'sum args) (length args)))

(defun list? (arg)
  "Synonym for listp."
  (listp arg))

(defun flatten-once (lst)
  "collects and inserts sublists of lst, leaving other elements untouched."
  (foldl (lambda (it ac)
		   (if (listp it) (append ac it)
			 (suffix ac it)))
		 nil
		 lst))

(defun flatten (lst)
  "Recursively flattens all sublists of lst and lst itself."
  (reverse
   (foldl
	(lambda (item output)
	  (cond ((list? item)
			 (foldl #'cons output (flatten item)))
			(t
			 (cons item output)))) '() lst)))

(defmacro after-this-line (&rest body)
  "Executes body of macro after moving the point forward two lines."
  `(progn (insert "\n\n")
		  ,@body))

(defmacro enclambda (what arglist &rest body)
  "Combines a lexical-let form with lambda.  Produces new lexical variables binding WHAT before a lambda is created."
  (let ((lexletarg
		 (foldl (lambda (cu ou) (cons (list cu cu) ou))
				'()
				what)))
	`(lexical-let ,lexletarg
	   (lambda ,arglist
		 ,@body))))

(defun null? (lst) "Returns true for an empty list." (eq '() lst))

(defun any (list)
  "Returns true if any of lst is true.  Stops at first true."
  (let ((b nil))
	(loop for item in list do
		  (setq b (or b item)))
	b))

(defun all (list)
  "Returns true if all of list is true."
  (let ((b t))
	(loop for item in list do
		  (setq b (and b item)))
	b))

(defun none (list)
  "Returns true if none of list is true."
  (not (any list)))

(defun fix (f a0 &rest args)
  "Repeatedly applies f to its last result (initially f a0) until the result converges.  Not numerical.  Max it is optional argument."
  (let ((max-it (if (null? args) 100 (car args))))
	(let* ((prev (funcall f a0))
		   (current (funcall f prev)))
	  (loop while (not (equal prev current)) do
			(setf prev current)
			(setf current (funcall f prev)))
	  current)))


(defun bunch-list (lst)
  "Returns a new list with neighbors bunched into sublists of length 2."
  (reverse (cadr (foldl
				  (lambda (it ac) 
					(let ((ls (car ac))
						  (ol (cadr ac)))
					  (cond (ls (list nil (cons (list ls it) ol)))
							(t 
							 (list it ol)))))
				  (list nil nil)
				  lst))))

(defun tbl! (&rest args)
  "Creates or modifies a hash table.
  (tbl! old-table <sym1> <val1> ... <symN> <valN>) modifies table OLD-TABLE with new entries.
  (tbl! <sym1> <val1> ... <symN> <valN>) creates a new table with new entries.  
  Always returns the hash table."
  (cond 
   ((hash? (car args)) (foldl (lambda (pr tbl)
								(setf (cl-gethash (car pr) tbl)  (cadr pr))
								tbl)
							  (car args)
							  (bunch-list (cdr args))))
   (t
	(foldl (lambda (pr tbl)
			 (setf (cl-gethash (car pr) tbl)  (cadr pr))
			 tbl)
		   (cl-make-hash-table :test 'equal)
		   (bunch-list args)))))

(defun tbl (tbl &rest args)
  "Access a hash table.
  (tbl <tbl> el) returns the element in table.
  (tbl <tbl> el1 el2 el3) returns a list of the elements in question."
  (let ((res
		 (loop for arg in args collect (cl-gethash arg tbl))))
	(if (= 1 (length res))
		(car res)
	  res)))

(defsetf tbl tbl!)

(defun* tbl-or (tbl key &optional (otherwise nil))
  "Like tbl, but can only do one element and returns OTHERWISE when that is nil." 
  (cl-gethash key tbl otherwise))

(defun string-contains? (str re)
  "Returns true of the regexp RE matches STR."
  (let ((new (replace-regexp-in-string re "" str)))
	(not (string= new str))))

(defun keyshash (tbl)
  "Return a hash table's keys."
  (let ((keys '()))
	(maphash (lambda (k v) (push k keys)) tbl)
	(reverse keys)))

(defun valshash (tbl)
  "Return a hash table's values."
  (mapcar (lambda (k) (gethash k tbl)) (keyshash tbl)))

(defun hash? (o)
  "True for hash tables."
  (hash-table-p o))

(defun lmaphash (lam tbl)
  "Maps a function of KEY and VAL over KEYS and VALS of the hash table.  Returns a list."
  (let* ((keys (keyshash tbl))
		 (vals
		  (loop for key in keys collect (gethash key tbl))))
	(mapcar* lam keys vals)))

(defun lmaphash-vals (lam tbl)
  "Maps a function over the hash table values, returns a list."
  (mapcar lam (valshash tbl)))

(defun* join (lst &optional (del " "))
  "Joins a LST of strings into a single string using delimiter DEL."
  (foldl (lambda (it ac)
		   (concat ac it))
		 "" 
		 (cons (car lst) (mapcar (lambda (x) (concat del x)) (cdr lst)))))

(defun filter (f lst)
  "Returns a list of elements of F for which (f item) is true."
  (reverse (foldl (lambda (it ac)
					(if (funcall f it) 
						(cons it ac)
					  ac))
				  '() lst)))

(defun chomp (str)
  "Perl-like chomp function to return a version of STR with no whitespace."
  (let ((s (if (symbolp str)(symbol-name str) str)))
	(save-excursion
	  (while (and
			  (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
			  (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
		(setq s (replace-match "" t nil s)))
	  (while (and
			  (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
			  (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
		(setq s (replace-match "" t nil s))))
	s))

(defun build-on (machines body)
  "Constructs the body of the on macro."
  `(if (any (mapcar (lambda (x)
					  (string= system-name (format "%s" x)))
					,machines))
	   (progn
		 ,@body)))

(defmacro on (machines &rest body)
  "Perform an action only when system name is in the list of MACHINES."
  (build-on machines body))

(defmacro* place-case (&rest pairs)
  "Case expression where the system name is the value.  Perform actions only on certain systems."
  `(case (quote ,(intern system-name))
	 ,@(loop for pair in pairs collect
			 (cond 
			  ((stringp (car pair))
			   (cons `(quote ,(intern (car pair)))
					 (cdr pair)))
			  ((symbolp (car pair))
			   (cons `(quote ,(car pair))
					 (cdr pair)))
			  (t (error "place-case needs places enumerated as either strings or symbols"))))))

(defmacro defvar-buf-loc (nm &optional vl do)
  "def a var and immediately market it buffer local."
  `(progn (defvar ,nm ,vl ,do)
		  (make-variable-buffer-local ',nm)))

(defmacro defcustom-buf-loc (nm &optional vl do)
  "def a custom and immediately declare it buffer local."
  `(progn (defcustom ,nm ,vl ,do)
		  (make-variable-buffer-local ',nm)))

(defun comp (&rest fs)
  "Function composition."
  (lexical-let
	  ((rfs (reverse fs)))
	(lambda (&rest args)
	  (car (foldl
			(lambda (it ac)
			  (list (apply it ac)))
			args
			rfs)))))

(defmacro* let-seq (symbols lst &body body)
  "Sequential let form.  (let (a b c) '(1 2 3) (list a b c)) -> (1 2 3).
   binds variables to elements in a list."
  (let ((list-name (gensym "list-")))
	`(let ((,list-name ,lst))
	   (let ,(loop for i from 0 below (length symbols)
				   and s in symbols collect
				   (list s `(elt ,list-name ,i)))
		 ,@body))))

(defmacro* let-tbl (symbol-key-pairs tbl &body body)
  "Binds variables to elements of a hash table."
  (let ((tbl-name (gensym "table-")))
	`(let ((,tbl-name ,tbl))
	   (let ,(loop for i from 0 below (length symbol-key-pairs)
				   and sk in symbol-key-pairs collect
				   (list (car sk) `(tbl ,tbl-name ,(cadr sk))))
		 ,@body))))

(defmacro* llet-seq (symbols lst &body body)
  "Binds SYMBOLS to elements of LST lexically, in BODY."
  (let ((list-name (gensym "list-")))
	`(let ((,list-name ,lst))
	   (lexical-let ,(loop for i from 0 below (length symbols)
						   and s in symbols collect
						   (list s `(elt ,list-name ,i)))
		 ,@body))))

(defmacro* llet-tbl (symbol-key-pairs tbl &body body)
  "Binds by SYMBOL-KEY-PAIRS with values in TBL lexically in BODY."
  (let ((tbl-name (gensym "table-")))
	`(let ((,tbl-name ,tbl))
	   (lexical-let ,(loop for i from 0 below (length symbol-key-pairs)
						   and sk in symbol-key-pairs collect
						   (list (car sk) `(tbl ,tbl-name ,(cadr sk))))
		 ,@body))))


(defun elts (sq inds)
  "Returns a list of elements of the sequence SQ and indexes INDS."
  (loop for i from 0 below (length inds) 
		collect (elt sq (elt inds i))))


(defun split-list-left (lst pred)
  "Split a list at the first place (PRED ITEM) is true, true item goes into the first list."
  (if (not (functionp pred))
	  (split-list-left lst (lexical-let ((p pred)) (lambda (x) (eq p x))))
	(loop with found = nil
		  for i from 0 below (length lst)
		  when (not found)
		  collect (elt lst i) into before
		  when found
		  collect (elt lst i) into after 
		  when (funcall pred (elt lst i))
		  do (setf found t)
		  finally (return (list before after)))))

(defun split-list-right (lst pred)
  "Split a list at the first place (PRED ITEM) is true, true item goes into the second list."
  (if (not (functionp pred))
	  (split-list-right lst (lexical-let ((p pred)) (lambda (x) (eq p x))))
	(loop with found = nil
		  for i from 0 below (length lst)
		  when (funcall pred (elt lst i))
		  do (setf found t)
		  when (not found)
		  collect (elt lst i) into before
		  when found
		  collect (elt lst i) into after 
		  finally (return (list before after)))))

(defun split-list-drop (lst pred)
  "Split a list at the first place (PRED ITEM) is true, drop the true item."
  (if (not (functionp pred))
	  (split-list-drop lst (lexical-let ((p pred)) (lambda (x) (eq p x))))
	(loop with found = nil
		  for i from 0 below (length lst)
		  when (funcall pred (elt lst i))
		  do (setf found t)
		  when (not found)
		  collect (elt lst i) into before
		  when found
		  collect (elt lst i) into after 
		  finally (return (if (not found) nil (list before (cdr after)))))))


(defun v-last (v)
  "Last element of a vector."
  (elt v (- (length v) 1)))
(defun v-next-to-last (v)
  "Second to last element of a vector."
  (elt v (- (length v) 2)))
(defun v-rest (v)
  "all but first element of a vector."
  (apply #'vector 
		 (loop for i from 1 below (length v)
			   collect (elt v i))))

(defun* elt-or (seq n &optional (otherwise nil))
  "If ELT is out of range for SEQ, return OTHERWISE."
  (if (< n (length seq)) (elt seq n) otherwise))

(defun* elts-or (seq ns &optional (otherwises nil))
  "if any ELTS is out of range of SEQS return those OTHERWISES."
  (loop for it in ns collect (elt-or seq it (elt-or otherwises it nil))))

;; (elts-or '(10 9 8 7 6) '(0 1 2 3 15 16 0) '(20 20 20 20 20 20 20 20 20 20 20 20 20 20 220 20 20 20 20 20 20))
;; (elt-or [a b c] 3 'd)
;; (elt [a b c] 1)

;; (split-list-left '(1 2 3 4 5) 4)
;; (split-list-right '(1 2 3 4 5) 3)
;; (split-list-drop '(1 2 3 4 5) 3)

(defmacro comment  (&rest rest) "Don't do any of the code in here." 'nil)

(defun seq-type (sq)
  "Returns the type of a seq."
  (cond
   ((listp sq) 'list)
   ((vectorp sq) 'vector)))

(defun nthcdr-preserve-type (n sq)
  "Grabs the nthcdr of a seq, but converts the result to the seq type."
  (coerce 
   (nthcdr n (coerce sq 'list))
   (seq-type sq)))

										; (nthcdr-preserve-type 3 [0 1 2 3 4 5 6 7])

(defun transplant-tail (to from)
  "Append the longer part of FROM onto TO.  If FROM is shorter than TO, this is the identity."
  (coerce 
   (loop for i from 0 below (max (length from) (length to)) collect
		 (elt-or to i (elt from i)))
   (seq-type to)))

										; (transplant-tail '(1 2 3 4) '(3 2 4 5 6))

(defun ff/line (filename line-number)
  "Find-file FILENAME and jump to line LINE-NUMBER"
  (let ((buf (find-file filename)))
	(goto-line line-number)))

(defun ff/char (filename char-number)
  "Find-file FILE and jump to CHAR-NUMBER."
  (let ((buf (find-file filename)))
	(goto-char char-number)))


(defun put-string-on-kill-ring (string)
  "Put a string on the kill ring."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
	  (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring))

(defun ff/line->clipboard ()
  "Create the function call which jumps to this buffer and line, put it on the clipboard."
  (interactive)
  (let ((ln (line-number-at-pos))
		(filename
		 (buffer-file-name)))
	(put-string-on-kill-ring 
	 (format "(ff/line \"%s\" %d)" filename ln))))

(defun ff/char->clipboard ()
  "Create the function call which jumps to this buffer and char, put it on the clipboard."
  (interactive)
  (let ((pt (point))
		(filename
		 (buffer-file-name)))
	(put-string-on-kill-ring 
	 (format "(ff/char \"%s\" %d)" filename pt))))

(defun ff/this-text->clipboard (s e)
  "Create the function call which jumps to this text in this file, put it on the clipboard."
  (interactive "r")
  (put-string-on-kill-ring 
   (format 
	"(ff/this-text \"%s\" \"%s\")" 
	(buffer-file-name) 
	(buffer-substring-no-properties s e))))

(defun ff/this-text (filename txt)
  "Jump to the text TEXT in the file FILENAME."
  (with-current-buffer (find-file filename)
	(goto-char (point-min))
	(word-search-forward txt)))

(defun pwd->kill-ring ()
  "Put the working directory in the kill ring."
  (interactive)
  (put-string-on-kill-ring (pwd)))

(let ((currently-defining-defn 'range))
  (fset 'range
		(function
		 (lambda (&rest G1590)
		   "Range function.  (range 4) -> (0 1 2 3)
            (range 2 3) -> (2 3)
            (range 2 2 6) -> (2 4 6)"
		   (let ((G1591 (length G1590)))
			 (cond ((arity-match G1591 '(3 exactly))
					(lexical-let* ((lambda-seq-as-sym1608 G1590)
								   (start (elt lambda-seq-as-sym1608 0))
								   (step (elt lambda-seq-as-sym1608 1))
								   (upto (elt lambda-seq-as-sym1608 2)))
					  (loop for i from start below upto by step collect i)))
				   ((arity-match G1591 '(2 exactly))
					(lexical-let* ((lambda-seq-as-sym1621 G1590)
								   (start (elt lambda-seq-as-sym1621 0))
								   (upto (elt lambda-seq-as-sym1621 1)))
					  e
					  (range start 1 upto)))
				   ((arity-match G1591 '(1 exactly))
					(lexical-let* ((lambda-seq-as-sym1630 G1590)
								   (upto (elt lambda-seq-as-sym1630 0)))
					  (range 0 1 upto)))
				   (t (error "Unable to find an arity match for %d args in fn %s."
							 G1591
							 'lambda))))))))

(defmacro* $ (first f &rest rest)
  "Simple infix macro.  ($ a < b) -> (< a b)."
  `(,f ,first ,@rest))

(defun first (l) "Return first element of l." (car l))

(defun shell-to (dir)
  "Send the shell to the directory DIR.  List newest files there."
  (let* ((buf (shell))
		 (pro (get-buffer-process buf)))
	(send-string pro (concat "\ncd " dir "\n"))
	(with-current-buffer buf (cd dir))
	(send-string pro "ls -t | head -n 10\n")))

(defmacro* dont-do (&body body)
  "Don't do the body."
  `(progn nil))

(defun zip (&rest lsts)
  "map the list function over the lists given, produce a list of sublists."
  (apply 'mapcar* (cons 'list lsts)))

(defun evrep-region (start end)
  "Replace the region's lisp code with the result of evaluating it."
  (interactive "r")
  (let* ((str (buffer-substring-no-properties start end))
		 (v (eval (read str))))
	(kill-region start end)
	(insertf "%s" v)))

(defun e (x) "Exponential base e." (expt 10 x))

(defun buffers-matching (rx)
  "Show buffers matching a regular expression."
  (sort (filter 
		 (lambda (x) 
		   (string-match rx (buffer-name x)))
		 (buffer-list)) 
		(lambda (b1 b2)
		  (string< (buffer-name b1)
				   (buffer-name b2)))))
(defun print-buffers-matching (rx)
  "Prints buffers matching RX"
  (interactive "sEnter a Pattern:")
  (let* ((bfrs 
		  (sort (filter 
				 (lambda (x) 
				   (string-match rx (buffer-name x)))
				 (buffer-list)) 
				(lambda (b1 b2)
				  (string< (buffer-name b1)
						   (buffer-name b2))))))
	(print (join (mapcar #'buffer-name bfrs) ","))))

(defun show-buffers-matching (rx)
  "Shows the buffers matching RX in newly created windows."
  (interactive "sEnter a Pattern:")
  (let* ((bfrs 
		  (sort (filter 
				 (lambda (x) 
				   (string-match rx (buffer-name x)))
				 (buffer-list)) 
				(lambda (b1 b2)
				  (string< (buffer-name b1)
						   (buffer-name b2))))))
	(set-window-buffer (selected-window) (car bfrs))
	(let* ((nbuf (length bfrs))
		   (w (selected-window))
		   (h (window-height w))
		   (split-height (/ h nbuf)))
	  (loop for buf in (cdr bfrs) do
			(setf w (split-window w split-height))
			(select-window w)
			(set-window-buffer w buf)))))



(defun insert-buffer-name ()
  "Inserts the name of the current buffer, eliding the extension if it is obviously there."
  (interactive)
  (insert (replace-regexp-in-string "\\..*$" "" (buffer-name))))



(defun strip-directory (dr)
  (cadr (split-string dr "Directory ")))

(defun insert-pwd ()
  "Insert the present directory into the buffer."
  (interactive)
  (insert (strip-directory (pwd))))

(defun make-shell (name do)
  "Make a shell named NAME doing DO."
  (let ((sh (shell name)))
	(comint-send-string sh do)
	sh))

(defun scheme-here ()
  "Start scheme here, in the PWD."
  (interactive)
  (switch-to-buffer (make-shell "scheme" "mred -z -e (current-directory (string->path \"~/\"))"))
  (inferior-scheme-mode))

(defun goog-prep (str)
  "Prepare a string for googling."
  (replace-regexp-in-string " " "+" str))

(defun gs (start end)
  "Google-search the region."
  (interactive "r")
  (let ((search (buffer-substring start end)))
	(shell-command (concat "chromium-browser \"http://www.google.com/search?hl=en&q=%22" (goog-prep search) "%22&btnG=Google+Search\""))))

(defmacro let-repeatedly (name &rest forms-to-apply)
  "Repeatedly bind the result of each FORM TO APPLY to NAME, in subsequent forms."
  `(let* ,(mapcar 
		   (lambda (f) 
			 `(,name ,f))
		   forms-to-apply)
	 ,name))

(defun build-list-of-forms (name forms)
  "Bults a list of forms for LET-REPEATEDLY-UNTIL."
  (loop for f in forms collect
		`(lambda (,name) ,f)))

(defun symbol-defined? (symbol)
  "Test to see if a symbol has a value."
  (let ((return-val nil))
	(condition-case nil (setq return-val (symbol-value symbol))
	  (error nil))))

(defmacro let-repeatedly-until (name pred &rest forms)
  "Like let-repeatedly, but stop once PRED is TRUE, returning last NAME value."
  (let ((function-list-name (gensym "function-list-"))
		(f-name (gensym "let-repeatedly-fun-"))
		(state-holder (gensym "let-repeatedly-state-holder-")))
	`(let ((,state-holder (symbol-defined? ',name)))
	   (let ((,function-list-name (list ,@(build-list-of-forms name forms))))
		 (loop for ,f-name in ,function-list-name
			   do 
			   (setq ,state-holder(funcall ,f-name ,state-holder))
			   while (not (funcall ,pred ,state-holder))
			   finally (return ,state-holder))))))


(defun* ok-today? (&optional (p .3))
  (> (/ (read (concat "#x" (substring (md5 (calendar-iso-date-string)) 0 2))) 255.0) p))

(defmacro & (fs &rest args)
  "Ellision macro.  FS is a single symbol representing functions to be composed before application to args.
   (& cdr&car '( (a b c) (d e f))) -> (b c)."
  (let* ((s (format "%s" fs))
		 (ff (split-string s "&"))
		 (fs (reverse (mapcar #'intern ff)))
		 (inside (cons (first fs) args)))
	(loop with form = inside 
		  for f in (cdr fs) do
		  (setf form (list f form))
		  finally (return form))))

(defun comint-send-strings (buf-or-proc &rest rest)
  "Sends multiple strings to BUF-OR-PROC, appending a newline to each."
  (let ((proc (if (bufferp buf-or-proc) (get-buffer-process buf-or-proc)
				buf-or-proc)))
	(loop for string in rest do
		  (comint-send-string proc (concat string "\n")))))


(defun* all-words (&optional (start (point-min)) (stop (point-max)))
  "Return all the words in the buffer by FORWARD and BACKWARD-WORD."
  (save-excursion 
	(goto-char start)
	(loop with last-pos = start
		  while (forward-word 1) collect
		  (prog1 (buffer-substring (save-excursion (backward-word) (point)) (point))
			(setq last-pos (point))))))

(defun insert-a-word ()
  "With ido-completion, insert a word from somewhere else in the buffer."
  (interactive)
  (insert (ido-completing-read "word: " (unique (all-words) #'string=) nil nil)))

(defun* vert-hist (bins labels data (&optional (max 50)))
  "Insert a vertical histogram of the DATA into BINS.  Mark with LABELS."
  (let ((counts (make-vector (length bins) 0)))
	(loop for point in data do
		  (loop for bin in bins and 
				i from 0
				while (not 
					   (and (>= point (car bin))
							(< point (cadr bin))))
				finally (setf (aref counts i)
							  (+ 1 (aref counts i)))))
	counts))

(defun internf (s &rest args)
  "Like intern, but with format semantics for the args."
  (intern (apply #'format (cons s args))))

(defun insert-time ()
  "Inserts the current time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))

(defun word-list (s)
  "Split a string on spaces into words, trim and chomp elements."
  (mapcar #'org-trim (split-string s " ")))

(defun functional-sort (list pred)
  "Sorts LIST with PRED functionally."
  (sort (copy-sequence list) pred))

(defun wd ()
  "Like pwd, but returns just the directory."
  (car (sh "pwd")))

(defun files-wd (&rest rest)
  "List the files here."
  (apply #'directory-files (cons (wd) rest)))

(dont-do
 (wd))



(defun* alist (alist el)
  "Access element EL in an alist ALIST."
  (cadr (assoc el alist)))

(defun alist! (alist el value)
  "Destructively updates EL to VALUE in ALIST."
  (let ((element-holder (assoc el alist)))
	(if element-holder (setf (cadr element-holder) value)
	  (setcdr (last alist) (list (list el value)))))
  alist)

(defsetf alist alist!)

(defun* alist-or (alist el &optional (or-val nil))
  "Like ALIST but returns OR-VAL if (alist lst el) is nil."
  (let ((v (assoc el alist)))
	(if v v or-val)))
(defun* qalist (alist el)
  "Like ALIST but uses assq for efficiency."
  (cdr (assq el alist)))
(defun* qalist-or (alist el &optional (or-val nil))
  "Like QALIST but supports OR-VAL."
  (let ((v (assq el alist)))
	(if v v or-val)))

(defun sub-alist (alist keys)
  "Produces a new ALIST with only KEYS from ALIST."
  (mapcar 
   (lambda (key)
	 (list key (alist alist key)))
   keys))

(defun alist-in (root keys)
  "Gets a value from a nested set of alist using a sequence of keys.  Returns the val."
  (foldl (lambda (it ac)
		   (alist ac it))
		 root
		 keys))

(defun alist>>-in (root keys val)
  "Sets a value from a nested set of alists using a list of keys.  Returns new alist.  Functional."
  (if (= (length keys) 1) (alist>> (car keys) val)
	(alist>> root (car keys)
			 (alist>>-in (alist root (car keys)) (cdr keys) val))))

(defun alist-conjugate (alst key fun)
  "Returns a new alist where the value of key is now (fun (alist alst key))."
  (let ((val (alist alst key)))
	(alist>> alst key (funcall fun val))))

(defun alist-cons (alst key value)
  "Cons the element VALUE onto the list at KEY in ALST.  If key is not there, obviously this creates a list there."
  (alist-conjugate alst key 
				   (lexical-let ((value value))
					 (lambda (xxx) (cons value xxx)))))

(defun alist-add-to-set (alst key value)
  (alist-conjugate alst key
				   (lexical-let ((value value))
					 (lambda (xxx) (if (not ($ value in xxx)) (cons value xxx) xxx)))))

(defun dissoc (alist &rest keys)
  "Returns a new ALIST without KEYS."
  (let ((keys (flatten keys)))
	(loop for element in alist when
		  (let ((alist-el-key 
				 (if (listp element)
					 (car element)
				   element)))
			(not ($ alist-el-key in keys)))
		  collect element)))

(defun dissoc-equal (alist &rest keys)
  "Returns a new ALIST without KEYS.  Use EQUAL for equality."
  (let ((keys (flatten keys)))
	(loop for element in alist when
		  (let ((alist-el-key 
				 (if (listp element)
					 (car element)
				   element)))
			(not ($ alist-el-key in keys #'equal)))
		  collect element)))


(defun* alist>> (&optional alist &rest rest)
  "Create or functionally modifies an ALIST.
   (alist>> alist [key val]...) adds key vals to the alist.
   (alist>> [key val]...) returns a new alist with keys and vals."
  (cond 
   ((and (eq nil alist)
		 (eq nil rest))
	nil)
   ((and (listp alist)
		 (eq nil rest))
	alist)
   ((and (not (listp alist))
		 (not (eq nil rest)))
	(foldl #'cons nil (reverse (bunch-list (cons alist rest)))))
   ((and (listp alist)
		 (not (eq nil rest)))
	(let* ((pairs (bunch-list rest))
		   (symbols (mapcar #'car pairs))
		   (dalist (dissoc alist symbols)))
	  (foldl #'cons dalist (reverse (bunch-list rest)))))))

(defun* alist-equal>> (&optional alist &rest rest)
  "Create or functionally modifies an ALIST.
   (alist>> alist [key val]...) adds key vals to the alist.
   (alist>> [key val]...) returns a new alist with keys and vals. Use EQUAL for redundancy checking."
  (cond 
   ((and (equal nil alist)
		 (equal nil rest))
	nil)
   ((and (listp alist)
		 (equal nil rest))
	alist)
   ((and (not (listp alist))
		 (not (equal nil rest)))
	(foldl #'cons nil (reverse (bunch-list (cons alist rest)))))
   ((and (listp alist)
		 (not (equal nil rest)))
	(let* ((pairs (bunch-list rest))
		   (symbols (mapcar #'car pairs))
		   (dalist (dissoc-equal alist symbols)))
	  (foldl #'cons dalist (reverse (bunch-list rest)))))))


(defun alist-keys (alist)
  "Just the alist-keys."
  (mapcar #'car alist))

(defmacro eq-commute (fun a b)
  "Apply a fun to a and b before testing for equality."
  `(eq (funcall ,fun ,a) (funcall ,fun ,b)))
(defmacro bool-commute (comp fun a b)
  "Comppose the functions which result from applying fun to a and b."
  `(,comp (funcall ,fun ,a) (funcall ,fun ,b)))

(defun get-last-sexp ()
  "Grab the last sexp from this point."
  (read (buffer-substring (save-excursion (backward-sexp 1) (point))
						  (point))))

(defun macroexpand-eval-last-sexp ()
  "Eval the last sexp but macro-expand it first."
  (interactive)
  (print (eval (macroexpand-all (get-last-sexp)))))

(global-set-key [\C-ce] 'macroexpand-eval-last-sexp)


;; (if alist
;; 	  (if (not (listp alist))
;; 		  (apply #'alist>> (cons nil (cons alist rest)))
;; 		(foldl #'cons alist (reverse (bunch-list rest))))
;; 	alist))

;; (defun alist>> (&rest rest)
;;   (let ((narg (length rest)))
;; 	(cond
;; 	 ((= 0 narg) nil)
;; 	 ((> narg 0)
;; 	  (let ((alist (if (listp (car rest)) 
;; 					   (let ((alist (pop rest)))
;; 						 (dissoc alist 
;; 								 (loop for it in rest and
;; 									   i from 0 when (evenp i)
;; 									   collect it))								 
;; 						 nil))))
;; 			(append (bunch-list rest) alist))))))

(defun alist-fields (alist)
  "Get the field names of an alist."
  (mapcar #'car alist))

(defun alist-inp (list-element key)
  "Predicate to detect if a part of a alist matches key."
  (if (listp list-element)
	  (equal (car list-element) key)
	(equal list-element key)))

(defun and-over (pred lst)
  "Reduce the application of pred to the elements of lst with AND."
  (foldl (lambda (it ac)
		   (and (funcall pred it) ac))
		 t
		 lst))


(defun or-over (pred lst)
  "Reduce the application of pred to the elements of lst with OR."
  (foldl (lambda (it ac)
		   (or (funcall pred it) ac))
		 nil
		 lst))

(defun permute-list (lst)
  "Return a random-enough arrangement of the elements in LST."
  (sort* (copy-list lst)
		 (lambda (a b)
		   (< (random) (random)))))

(defun buffer-line ()
  "Returns the current line of the current buffer as a string."
  (buffer-substring-no-properties (get-beginning-of-line) (get-end-of-line)))

(defun buffer-all-lines ()
  "Returns all the lines in a buffer as a list."
  (save-excursion (goto-char (point-min))
				  (loop collect
						(buffer-line)
						while (= (forward-line 1) 0))))


(defun org-line->list (str)
  "Split an org-mode-table line in STR int a list."
  (mapcar #'chomp (split-string str (regexp-quote "|"))))

(defun cleave ( list-of-funs args )
  "Apply the list of functions to ARGS and return a list of Results.  Bizzaro mapcar."
  (let ((args (if (listp args) args (list args))))
	(loop for f in list-of-funs collect
		  (apply f args))))



(defun* capture-shell (command &optional (args ""))
  "Execute a shell command and return the output as a list of strings."
  (let* ((command-part (car (split-string command " ")))
		 (args (concat (replace-regexp-in-string command-part "" command)
					   args)))
	(chomp-lines (split-string (with-temp-buffer 
								 (call-process-shell-command command-part nil 
															 (buffer-name (current-buffer))
															 nil args)
								 (accept-process-output)
								 (buffer-substring (point-min) (point-max))) lb))))
(defun* sh (command &optional (args ""))
  "sh command args - Send a command to the shell, get back the result as a list of strings."
  (capture-shell command args))

(defmacro la (args &rest body)
  "Short form of lambda for the very lazy."
  `(lambda ,args ,@body))

(defun rxq (string)
  "rxq is a shorthand for regexp-quote."
  (regexp-quote string))

(defun reprxstr (rx rep str &optional fixedcase literal subexp start)
  "reprxstr is a shorthand for replace-regexp-in-string."
  (replace-regexp-in-string rx rep str fixedcase literal subexp start))

(defun uncapitalize (s)
  "Uncapitalize a word in string."
  (let ((first (substring s 0 1))
		(rest (substring s 1 (length s))))
	(concat (downcase first)
			rest)))

(defun remove-first-or-last-if (lst pred)
  "Remove the first element if it matches pred and/or remove the last element if it matches pred."
  (let-repeatedly lst 
				  (if (funcall pred (car lst)) (cdr lst) lst)
				  (if (funcall pred (car (last lst)))
					  (butlast lst 1) lst)))

(defun* chomp-lines (lst &optional (pred (lambda (x) (string= "" (chomp x)))))
  "removes leading and trailing spaces from lines, removes empty lines entirely if they are leading or trailing."
  (fix 
   (lambda (x) (remove-first-or-last-if x pred))
   lst))

(defun region->camelcase (start end)
  "Takes a lisp-style-symbol in the current region and camelCasesIt."
  (interactive "r")
  (let* ((reg (buffer-substring start end))
		 (rep (uncapitalize (join (mapcar
								   (lambda (x)
									 (capitalize x))
								   (split-string reg (rxq "_"))) ""))))
	(kill-region start end)
	(insert rep)))

(defun camel-case (string)
  "Convert lisp-style to camelCase."
  (let* ((parts (split-string string (rxq "-")))
		 (parts (cons (car parts)
					  (mapcar #'capitalize 
							  (cdr parts)))))
	(join parts "")))

(defun camel-case-kw (kw)
  "Converts a keyowrd to camelCase."
  (let ((s (format "%s" kw)))
	(intern (concat ":" (camel-case (substring s 1 (length s)))))))

(defun cd-shell ()
  "Change the working directory to whatever the shell is working on."
  (cd (with-current-buffer "*shell*"
		(wd))))

(defmacro* with-wd (d &body body)
  "Execute BODY with a temporary working directory."
  (let ((current-directory (gensym "current-directory-")))
	`(let ((,current-directory (wd)))
	   (cd ,d)
	   (prog1 
		   (progn ,@body)
		 (cd ,current-directory)))))

(defmacro* with-shell-directory (&body body)
  "Execute body in the working directory of the *shell* buffer."
  `(with-wd 
	(with-current-buffer "*shell*" (wd))
	,@body))

(defun shell-to-here ()
  "Move the *shell* to the current working directory."
  (interactive)
  (comint-send-strings (get-buffer "*shell*") (concat "cd " (wd))))

(defun concatf (strings &rest rest)
  "Concat strings then run the result through FORMAT with REST."
  (apply #'format (apply #'concat strings) rest))

(defun filter-by-index (pred list)
  "Filter a list by the indexes of the elements."
  (loop for item in list and index from 0 
		when (funcall pred index) collect item))

(defun odd-indexed-elements (list)
  "Just return the odd-indexed elements of the list."
  (filter-by-index #'oddp list))

(defun even-indexed-elements (list)
  "Just return the even-indexed elements of the list."
  (filter-by-index #'evenp list))

(defun none-nil (lst)
  (and-over #'identity lst))



(defun map&filter (filter-fun transform &rest lists)
  "Map TRANSFORM across elements in LISTS keeping only those for which FILTER-FUN is true on the output of TRANSFORM."
  (let ((rests lists)
		(output nil))
	(loop while (none-nil rests) do
		  (let* ((els (mapcar #'car rests))
				 (new-rests (mapcar #'cdr rests))
				 (val (apply transform els)))

			(setq rests new-rests)
			(if (funcall filter-fun val) (push val output))))
	(reverse output)))

(defun filter&map (filter-fun transform &rest lists)
  "Map TRANSFORM across ELEMENTS in LISTS only for those for which FILTER-FUN is true."
  (let ((rests lists)
		(output nil))
	(loop while (none-nil rests) do
		  (let* ((els (mapcar #'car rests))
				 (new-rests (mapcar #'cdr rests))
				 (check (apply filter-fun els)))
			(setq rests new-rests)
			(if check (push (apply transform els) output))))
	(reverse output)))

(defun factor (n)
  "factor a number n by recourse to the command line utility FACTOR."
  (mapcar #'read (cdr (split-string (car (capture-shell "factor" (format "%d" n))) " " t))))

(defun table-like-get (tbl-like kw)
  "Get from anything that is table-like."
  (cond ((hash-table-p tbl-like) (tbl tbl-like kw))
		((listp tbl-like) (cadr (assq kw tbl-like)))))
(defun* table-like-get-or (tbl-like kw &optional (or-val nil))
  "Get from anything that is table-like or return OR-VAL."
  (cond ((hash-table-p tbl-like) (tbl-or tbl-like kw or-val))
		((listp tbl-like) 
		 (let ((v (assoc-default kw tbl-like #'eq nil)))
		   (if v (car v) or-val)))))

(defun print-and-return (x)
  "Print something before returning it."
  (cl-prettyprint x)
  x)

(defmacro always (val)
  "Return a function which always returns val."
  (let ((s (gensym "always-"))
		(r (gensym "rest-")))
	`(lexical-let ((,s ,val))
	   (lambda (&rest ,r)
		 ,s))))

(defun cut-region-replace (s)
  "Replace the current region with s, putting the old value in the kill ring."
  (interactive "s")
  (kill-region (point) (mark))
  (insert s))


(defmacro* lex-lambda (arglist &body body)
  "Create a lambda with lexically bound arguments."
  (let* ((actual-args (filter 
					   (lambda (x) 
						 (let ((x (format "%s" x)))
						   (and (not (string= x "&rest"))
								(not (string= x "&optional")))))
					   arglist))
		 (lex-forms (mapcar (lambda (x) (list x x))
							actual-args)))
	`(lambda ,arglist
	   (lexical-let ,lex-forms ,@body))))

(defmacro* lex-defun (name arglist doc &body body)
  "Define a function with lexically bound arguments."
  (let* ((actual-args (filter 
					   (lambda (x) 
						 (let ((x (format "%s" x)))
						   (and (not (string= x "&rest"))
								(not (string= x "&optional")))))
					   arglist))
		 (lex-forms (mapcar (lambda (x) (list x x))
							actual-args)))
	(if (stringp doc)
		`(defun ,name ,arglist ,doc
		   (lexical-let ,lex-forms ,@body))
	  `(defun ,name ,arglist 
		 (lexical-let ,lex-forms ,doc ,@body)))))

(defmacro ix: (lst indexes) 
  "Return the elements from LST matching INDEXES.  For convenience END and END+ are bound locally to (length lst) -1 and (length lst) respectively."
  (let ((lst-sym (gensym "ix-lst")))
	`(let* ((,lst-sym ,lst)
			(end (- (length ,lst-sym) 1))
			(end+ (length ,lst-sym)))
	   (elts ,lst-sym ,indexes))))

(defun shf (command-string &rest args)
  "Send command to a shell, but string has format semantics."
  (sh (apply #'format command-string args)))

(defun printf (&rest args)
  "Like print, but with format semantics on args."
  (print (apply #'format args)))

(defun buffer-next-word ()
  "Get the next word from a buffer."
  (let ((s (point))
		(q (forward-word 1)))
	(chomp (buffer-substring s (point)))))

(defvar double-quote "\"" "Double quote string.")

(defun shell-quote (x)
  "Quote a string for the shell."
  (concat double-quote x double-quote))

(defun escapce-spaces (s)
  "Puts escape characters in front of quotes."
  (replace-regexp-in-string " " "\\\\ " s))

(defun as-string (item) "Convert anything to a string using %s" (format "%s" item))
(defun as-string-readable (item) "Convert anything to a string using %S" (format "%S" item))

(defun concat* (&rest args)
  "Like concat, but first converts all args to strings."
  (apply #'concat 
		 (mapcar #'as-string args)))

(defun list-of-strings (lst)
  "Convert all elements of LST to strings"
  (and (listp lst)
	   (all (mapcar #'stringp lst))))

(defun line-min ()
  "Get point corresponding to beginning of line."
  (save-excursion 
	(beginning-of-line) (point)))

(defun line-max ()
  "Get point corresponding to end of line."
  (save-excursion 
	(end-of-line) (point)))

(defmacro string-case (expr &rest cases)
  "Case macro for string values."
  (let ((expr-sym (gensym "string-case-sym-")))
	`(let ((,expr-sym ,expr))
	   (cond
		,@(mapcar (lambda (x)
					(let* ((case-list (car x))
						   (case-list 
							(if (list-of-strings case-list) case-list
							  (list case-list)))
						   (expr (cdr x)))
					  `((or ,@(mapcar 
							   (lambda (s) `(string= ,expr-sym ,s))
							   case-list)) ,@expr)))
				  cases)))))

(defun buffer-subline ()
  "Get the current line in the buffer."
  (buffer-substring (line-min) (line-max)))
(defun buffer-subline-no-properties ()
  "Get the current line in the buffer without properties."
  (buffer-substring-no-properties (line-min) (line-max)))

(defun not-f (f)
  "Return a function which is the composition of not and f."
  (lexical-let ((f f))
	(lambda (&rest args) (not (apply f args)))))

(defun empty? (x)
  "True if length of X is zero."
  (= 0 (length x)))

(defmacro* with-current-buffer/save-excursion (buffer &body body)
  "Macro composition of with-current-buffer and save-excursion."
  `(with-current-buffer ,buffer 
	 (save-excursion ,@body)))

(defun f-not (f)
  "Composes not and f"
  (lexical-let ((lf f))
	(lambda (&rest args) (lexical-let ((largs args))  (not (apply lf largs))))))

(defun f-kw (kw)
  "Returns a function which pulls KW from a table."
  (lexical-let ((lkw kw))
	(lambda (tbl) (table-like-get-or  tbl lkw))))

(defun replace-string-in-string (str rep input)
  "Replace STR with REP in INPUT."
  (replace-regexp-in-string (rxq str) rep input))

(defmacro let-with-errors (bind/conditions &rest body)
  "A let form with built-in error checks following the usual binding forms.
  (let ((sym val error-predicate error-message)
        (sym1 val2 ...))
     BODY)
  Error predicate and error-message are optional.
  Example:
  (let ((x 10 #'numberp \"x must be a number\"))
     (+ x 11))"
  `(let ,(mapcar 
		  (lambda (bind/condition)
			(if (symbolp bind/condition) bind/condition)
			(let ((n (length bind/condition)))
			  (if (not (or (= n 2)
						   (= n 4)))
				  (error "let-with-errors needs binding forms with 2 or 4 parts <symbol> <value> <predicate> <error>."))
			  (cond ((= n 2) bind/condition)
					((= n 4)
					 (let ((sym (car bind/condition))
						   (val-expr (cadr bind/condition))
						   (predicate (caddr bind/condition))
						   (message (elt bind/condition 3))
						   (temp (gensym "bind-condition-temp-"))
						   (f-temp (gensym "bind-condition-temp-fun"))
						   (m-temp (gensym "bind-condition-temp-message")))
					   `(,sym (let ((,temp ,val-expr)
									(,f-temp ,predicate)
									(,m-temp ,message))
								(if (funcall ,f-temp ,temp)
									,temp
								  (error (format ,m-temp ,temp))))))))))
		  bind/conditions)
	 ,@body))

(defmacro let-with-errors* (bind/conditions &rest body)
  "A let* form with built-in error checks following the usual binding forms.
  (let ((sym val error-predicate error-message)
        (sym1 val2 ...))
     BODY)
  Error predicate and error-message are optional.
  Example:
  (let ((x 10 #'numberp \"x must be a number\"))
     (+ x 11))"
  `(let* ,(mapcar 
		   (lambda (bind/condition)
			 (if (symbolp bind/condition) bind/condition)
			 (let ((n (length bind/condition)))
			   (if (not (or (= n 2)
							(= n 4)))
				   (error "let-with-errors needs binding forms with 2 or 4 parts <symbol> <value> <predicate> <error>."))
			   (cond ((= n 2) bind/condition)
					 ((= n 4)
					  (let ((sym (car bind/condition))
							(val-expr (cadr bind/condition))
							(predicate (caddr bind/condition))
							(message (elt bind/condition 3))
							(temp (gensym "bind-condition-temp-"))
							(f-temp (gensym "bind-condition-temp-fun"))
							(m-temp (gensym "bind-condition-temp-message")))
						`(,sym (let ((,temp ,val-expr)
									 (,f-temp ,predicate)
									 (,m-temp ,message))
								 (if (funcall ,f-temp ,temp)
									 ,temp
								   (error (format ,m-temp ,temp))))))))))
		   bind/conditions)
	 ,@body))

(defmacro mapcar-lambda (lst args &rest body)
  "Combines mapcar and lambda.  List is passed first so that the body can end the function."
  `(mapcar (lambda ,args ,@body) ,lst))

(defun length=1 (lst) (= (length lst) 1))
(defun length=0 (lst) (= (length lst) 0))
(defun length-is (lst n) (= (length lst) n))
(defun length=1or0 (lst) (or (length=1 lst) (length=0 lst)))
(defun length=0or1 (lst) (or (length=1 lst) (length=0 lst)))
(defun is-&rest (object)
  (and (symbolp object)
	   (eq '&rest object)))
(defun is-&optional (object)
  (and (symbolp object)
	   (eq '&optional object)))

(defun break-list-on (pred lst)
  "Breaks a list on predicate.  Returns list of sublists.  Each time pred is true, a new list is started."
  (reverse (foldl 
			(lambda (item output)
			  (let ((current (car output))
					(other (cdr output)))
				(if (funcall pred item)
					(cons (list) (cons (reverse current) other))
				  (cons (cons item current) other))))
			()
			lst)))

(defun is-arg-list-sep (ob)
  "Returns true for arg list separators like &rest and &optional"
  (and (symbolp ob)
	   (or 
		(is-&optional ob)
		(is-&rest ob))))

(defun create-arg-alist (args)
  "Parses an arg list into an alist for use later."
  (let* ((seps (filter 
				(lambda (x)
				  (or (is-&rest x)
					  (is-&optional x)))
				args))
		 (parts (break-list-on #'is-arg-list-sep args)))
	(zip (cons 'regular seps) parts)))

(defun optional-before-rest? (arglist)
  "Make sure that optional does not occur before rest."
  (or 
   ($ (length (member '&rest arglist)) < (length (member '&optional arglist)))
   (= 0 (length (member '&optional arglist)))
   (= 0 (length (member '&rest arglist)))))

(defun check-and-return-arg-alist (args)
  "Check an arglist and return an alist."
  (assert (optional-before-rest? args) "&optional must be before &rest in order for an alist to make sense.")
  (let ((arg-alist (create-arg-alist args)))
	(assert (length=0or1 (alist arg-alist '&rest)) "&rest can only accept a single argument.")
	arg-alist))

(defun car-if-list-else-id (item)
  "Grab the first element of a list or just return the thing itself."
  (if (listp item) (car item)
	item))



(defun reconstitute-standard-arg-list (arg-alist)
  "Rebuild an arglist from an arglist with error checking annotations."
  (append 
   (reverse (reconstitute-regular-args arg-alist))
   (reconstitute-optional-args arg-alist)
   (reconstitute-rest-args arg-alist)))


(defmacro let-if (name pred true-branch false-branch)
  "Execute an if-statement with NAME bound to the result of PRED in BRANCHES."
  `(let ((,name ,pred))
	 (if ,name ,true-branch ,false-branch)))

(defun reconstitute-regular-args (arg-alist)
  "Rebuild the regular part of an arglist from the annotated parsed alist."
  (let-if regulars (alist arg-alist 'regular)
		  (mapcar #'car-if-list-else-id regulars)
		  nil))
(defun reconstitute-optional-args (arg-list)
  "Rebuild the optional part of an arglist from the annotated parsed alist."
  (let-if optionals (alist arg-list '&optional)
		  (cons '&optional (mapcar #'car-if-list-else-id optionals))
		  nil))
(defun reconstitute-rest-args (arg-list)
  "Rebuild the rest part of an arglist from the annotated parsed alist."
  (let-if rests (alist arg-list '&rest)
		  (cons '&rest (mapcar #'car-if-list-else-id rests))
		  nil))

(defun generate-error-checking-statements (arg-alist)
  "Build the error checking statements for a defun-checked."
  (append 
   (generate-error-checking-statements-regular arg-alist)
   (generate-error-checking-statements-optional arg-alist)
   (generate-error-checking-statements-rest arg-alist)))

(defun generate-error-checking-statements-regular (arg-list)
  "Generate the regular argument error checking statements."
  (foldl 
   (lambda (it ac)
	 (if (symbolp it)
		 ac
	   (let* ((sym (car it))
			  (error-check-expr (cadr it))
			  (report-string (caddr it)))
		 (if (not report-string) 
			 (setq report-string ""))
		 (if (not error-check-expr)
			 (setq error-check-expr (always t)))
		 (cons `(if (not (funcall ,error-check-expr ,sym))
					(error (concat (format "%s: " *defun-checked-name-lex*) ,report-string) ,sym)) ac))))
   ()
   (reverse (alist arg-list 'regular))))

(defun generate-error-checking-statements-optional (arg-list)
  "Generate the optional argument error checking statements."
  (foldl 
   (lambda (it ac)
	 (if (symbolp it)
		 ac
	   (let* ((sym (car it))
			  (error-check-expr (elt it 2))
			  (report-string (elt it 3)))
		 (if (not report-string) 
			 (setq report-string ""))
		 (if (not error-check-expr)
			 (setq error-check-expr (always t)))
		 (cons `(if (not (funcall ,error-check-expr ,sym))
					(error (concat (format "%s: " *defun-checked-name-lex*) ,report-string) ,sym)) ac))))
   ()
   (reverse (alist arg-list '&optional))))

(defun generate-error-checking-statements-rest (arg-list)
  "Generate the rest part of the error checking statements."
  (let* ((rest-expr (car (alist arg-list '&rest))))
	(if (symbolp rest-expr) nil
	  (let* ((rest-name (car rest-expr))
			 (predicate-name (gensym "defun-checked-rest-predicate-"))
			 (predicate-expression (cadr rest-expr))
			 (possible-error-string (caddr rest-expr)))
		(if (functionp predicate-expression)
			`((if (not (funcall ,predicate-expression ,rest-name))
				  (error (concat (format "%s: " *defun-checked-name-lex*) ,possible-error-string) ,rest-name)))
		  (loop for sub-pred-expr in predicate-expression and
				i from 0 collect 
				(let ((predexpr (car sub-pred-expr))
					  (error-str (cadr sub-pred-expr)))
				  `(if (not (funcall ,sub-pred-expr (elt ,rest-expr ,i)))
					   (error (concat (format "%s: " *defun-checked-name-lex*) ,error-str (format "%s(%d)" ,rest-name ,i)))))))))))


(setq *defun-checked-name-lex* nil)


(defmacro defun-checked (name args &rest body)
  "Like defun but provides argument-list level error checking features."
  (assert (length=1or0 (filter #'is-&rest args)) "defun-checked args can contain only 1 &rest element.")
  (assert (length=1or0 (filter #'is-&optional args)) "defun-checked args can contain only 1 &optional element.")
  (let* ((arg-alist
		  (check-and-return-arg-alist args))
		 (standard-arg-list (reconstitute-standard-arg-list arg-alist))
		 (error-checks (generate-error-checking-statements arg-alist)))
	`(lexical-let ((*defun-checked-name-lex* ',name))
	   (defun* ,name ,standard-arg-list ,@error-checks ,@body))))




(defun show-current-time ()
  "Show the current time."
  (interactive)
  (print (current-time-string)))

(defvar *after-select-window-hooks* nil "Hooks to execute when a buffer is entered.")
(defadvice select-window (after select-window-hooks ())
  (loop for hook in *after-select-window-hooks* do
		(funcall hook)))
(ad-activate 'select-window)

(defmacro with-write-temp-file (&rest body)
  "Execute body with a temp file, write to it, and return the name and the resutl in a list."
  (let ((file-name-name (gensym "file-name-"))
		(buffer-name    (gensym "buffer-")))
	`(let* ((,file-name-name (make-temp-file (format "%s" (gensym))))
			(,buffer-name (find-file-noselect ,file-name-name)))
	   (prog1 (with-current-buffer ,buffer-name
				(prog1 (list (progn ,@body)
							 ,file-name-name)
				  (save-buffer)
				  ))
		 (kill-buffer ,buffer-name)))))

(defun != (a b) "Not numerically equal." (not (= a b)))

(defun buffer-list-of-lines ()
  "Return all the lines in a buffer as alist."
  (save-excursion 
	(goto-char (point-min))
	(loop
	 collect (buffer-subline-no-properties)
	 while (= 0 (forward-line 1)))))

(defun nautilus-here ()
  "Launch nautilus here."
  (shf "nautilus %s &" (wd)))

(defun nautilus (s)
  "Launch nautilus wherever."
  (interactive "D")
  (shf "nautilus %s &" s))

(defun unfold (pred f gen init)
  "The unfold combinator.  Call GEN repeatedly on its result (starting with init) and collect the results of F on that value until PRED on that value is nil."
  (let ((output nil))
	(loop while (funcall pred init) do
		  (push (funcall f init) output)
		  (setq init (funcall gen init)))
	output))

(defun unfold-mem (pred f gen init)
  "The unfold combinator.  Call GEN repeatedly on its result (starting with init) and collect the results of F on that value until PRED all previous values of F on value is nil."
  (let ((output nil))
	(loop while (apply pred output) do
		  (push (funcall f init) output)
		  (setq init (funcall gen init)))
	output))

(defconst pi 3.14159265 "The constant pi.")
(defconst phi 1.61803399 "The golden ratio")

(defun gensymf (&rest args)
  "Like gensym with format semantics on arguments."
  (gensym (apply #'format args)))

(defun* for-work-monitor (&optional (val 100))
  "resize the text on screen for work monitor."
  (interactive)
  (set-face-attribute 'default nil :height val))

(defun zero? (n)
  "Is N zero?"
  (= n 0))

(defun positive? (n)
  "Is n positive?"
  (and (not (zero? n))
	   (= (abs n) n)))

(defun negative? (n)
  "Is n negative?"
  (and (not (zero? n))
	   (not (positive? n))))

(defun keyword->symbol (kw)
  "Convert a keyword to a regular symbol."
  (intern (substring (format "%s" kw) 1)))



(provide 'utils)
