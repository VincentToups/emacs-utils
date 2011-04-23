(require 'with-stack)
(require 'utils)
(require 'monads)
(require 'functional)
(require 'cl)

(defun prefix (x lst) (cons x lst))

(||| word: head&tail '(1>car) '(1>cdr) bi end:)
(||| word: tail&head head&tail swap end:)
(||| word: map-get-next-item rot tail&head '(-rot) dip end:)
(||| word: map ;( seq qtn -- newseq )
	 nil
	 '(map-get-next-item pick call swap 2>cons pick 1>length 0 2>= 1>not) loop
	 '(2drop) dip 1>reverse
	 end:)
(||| word: foldl ;( list init qtn -- result )
	 swapd leach end:)
(||| word: reduce ;( list qtn -- result )
     '(tail&head) dip foldl end:)
(||| word: sum ;( list -- sum )
     '(+) reduce end:)
 
(defstackword current-continuation 
  (push *stack* *stack*))

(defstackword apply-emacs-fun 
  (let ((arg-list (pop *stack*))
		(fun (pop *stack*)))
	(push (apply fun arg-list) *stack*)))

(defstackword alist>> 
  (let ((args (pop *stack*)))
	(push (apply #'alist>> (cons nil args)) *stack*)))

(defstackword alist 
  (|||- 2>alist))

(defstackword format ; ( format-string arguments -- string )
  (|||- cons 'format swap apply-emacs-fun))

(||| word: if* pick '(drop call) '(2nip call) if end:)

(defstackword cond ; ( association-list -- ...)
  (let ((condition-pairs (pop *stack*))
		(done nil))
	(loop while (not done)
		  for cond-pair
		  in condition-pairs do
		  (let ((condquot (car cond-pair))
				(doquot   (cadr cond-pair)))
			(if (|||- {condquot} call)
				(progn 
				  (|||- drop {doquot} call)
				  (setq done t))
			  (|||- drop))))))

(defn split-by-match 
  ([begin end [first & rest :as lst] 
		  height
		  outlist]
   (cond (lst
		  (cond
		   ((equal first begin)
			(recur begin end rest
				   (+ 1 height)
				   (cons first outlist)))
		   ((equal first end)
			(cond 
			 ((= height 1)
			  (Just (list (cdr (reverse (cdr (cons first outlist)))) rest)))
			 (t (recur begin end rest
					   (- height 1)
					   (cons first outlist)))))
		   (t (recur begin end rest
					 height
					 (cons first outlist)))))
		 ((not lst)
		  (None))))
  ([begin end lst]
   (split-by-match begin end lst 0 nil)))

(defstackword split-by-match 
  (let ((end (pop-stack))
		(begin (pop-stack)))
	(push-stack (split-by-match begin end (pop-stack)))))

(defstackword-immediate {/
  (let ((result (split-by-match '{/ '/} (cons '{/ *stack*))))
	(if (None? result) (error "Unmatched {/ during immediate word {/")
	  (let-seq (quot rest) (MaybeVal result)
;			   (db-print quot)
;			   (db-print rest)
			   (setq *stack* rest)
;			   (print *stack*)
			   (push `(quote ,quot) *stack*)
;			   (print *stack*)
))))

(defstackword-immediate {
  (let ((result (split-by-match '{ '} (cons '{ *stack*))))
	(if (None? result) (error "Unmatched { during immediate word {")
	  (let-seq (quot rest) (MaybeVal result)
;			   (db-print quot)
;			   (db-print rest)
			   (setq *stack* rest)
;			   (print *stack*)
			   (push `(quote ,quot) *stack*)
			   ;(print *stack*)
))))

(defstackword-immediate {:
  (let ((result (split-by-match '{: ':} (cons '{: *stack*))))
	(if (None? result) (error "Unmatched {: during immediate word {:")
	  (let-seq (quot rest) (MaybeVal result)
;			   (db-print quot)
;			   (db-print rest)
			   (setq *stack* rest)
;			   (print *stack*)
;			   (push `(quote ,quot) *stack*)
			   ;(print *stack*)
))))


(defstackword-immediate {-
  (let ((result (split-by-match '{- '-} (cons '{- *stack*))))
	(if (None? result) (error "Unmatched {- during immediate word {-")
	  (let-seq (contents future) (MaybeVal result)
			   (let ((head (car contents))
					 (tail (cdr contents)))
				 (setq *stack* (append (suffix tail head) future)))))))


(defstackword list-until  
  (let ((sentinal (pop *stack*)))
	(loop with output = nil 
		  while (not (eq (car *stack*) sentinal))
		  do
		  (if *stack* (setq output (cons (pop *stack*) output))
			(error "Couldn't find sentinal."))
		  finally 
		  (pop *stack*)
		  (push output *stack*))))

(defstackword-immediate {{ 
  (let ((result (split-by-match '{{ '}} (cons '{{ *stack*)))
		(list-sentinal (gensym "list-sentinal-")))
	(if (None? result) (error "Unmatched {{ during immediate word {{")
	  (let-seq (quot rest) (MaybeVal result)
			   (setq *stack* rest)
			   (setf *stack* (append `(',list-sentinal list-until) *stack*))
			   (push 'call *stack*)
			   (push `(quote ,quot) *stack*)
			   (push `(quote ,list-sentinal) *stack*)
			   ))))

(setq *stack-effect-sigil* -209092410)

(defun stack-effect (n)
  (vector *stack-effect-sigil* n))
(defun stack-effect? (n)
  (and (vector n)
	   (eq (elt n 0) *stack-effect-sigil*)))
(defun stack-effect-val (n)
  (if (stack-effect? n) (elt n 1)
	(error "%s is not a stack effect" n)))

(defun unknown-stack-effect? (part)
  (cond ((stack-effect? part)
		 (eq (stack-effect-val part) '*))
		(t
		 (and (= 1 (length part))
			  (eq (car part) '*)))))

(defun parse-stack-effect (se)
  (if (not (in '-- se)) (error "Improper stack effect documentation %s" se)
	(let-seq (in out) (split-list-drop 
					   se 
					   (lambda (x) (eq x '--)))
			 (if (or (unknown-stack-effect? in)
					 (unknown-stack-effect? out)) (stack-effect '*)
			   (stack-effect (- (length out) (length in)))))))

(defn stack-effect+ 
  ([se1 se2]
   (if (or (unknown-stack-effect? se1)
		   (unknown-stack-effect? se2))
	   (stack-effect '*)
	 (stack-effect (+ (stack-effect-val se1) (stack-effect-val se2)))))
  ([se1 se2 & rest]
   (reduce #'stack-effect+ (append (list se1 se2) rest))))

(defstackword emacs-apply
  (|||- swap 2>apply))

(defstackword stack-depth 
  (push-stack (length *stack*)))

(defstackword drop-all 
  (setq *stack* nil))

(defstackword swons (|||- swap cons))

(bivalent-stack-words append suffix prefix elt concat)
(univalent-stack-words listp not)

(word: odd? 1>oddp)
(word: even? 1>evenp)

(word: for-each {: seq qtn -- :}
  { { tail&head } dip dup { call } dip 
    { dup length 0 = not } dip swap } loop drop drop)

(provide 'stack-words)
