(require 'ra-lists)
(require 'recur)

(defstruct ptbl buckets test)

(defalias 'ptbl? #'ptbl-p)

(defun* fresh-ptbl (&optional (n 107) (test #'equal))
  "Create a fresh persistent hash table with N (107) bins and
TEST for equality."
  (make-ptbl :buckets (ra:make-list n '())
			 :test test))

(defvar the-empty-ptbl (fresh-ptbl) "An empty EQUAL testing persistent hash table.")
(defvar the-empty-ptbl-eq (fresh-ptbl 107 #'eq) "An empty EQ testing persistent hash table.")

(defun ptbl-set-buckets (p b)
  "Set the BUCKETS part of a persistent table P to B."
  (make-ptbl :buckets b
			 :test (ptbl-test p)))

(defun ptbl-dip-buckets (p f)
  "Set the BUCKETS part of a persistent table P to (FUNCALL F B),
where B is the old buckets."
  (make-ptbl :buckets (funcall f (ptbl-buckets p))
			 :test (ptbl-test p)))

(defun ptbl-n-buckets (p)
  "Return the number of buckets in the table P."
  (ra:length (ptbl-buckets p)))

(recur-defun* ptbl-add-to-bucket (bucket key val test &optional acc)
  "Add an association to BUCKET with KEY, VAL and KEY equality
tested under TEST."
  (cond ((empty? bucket) (cons (cons key val) acc))
		(t
		 (let* ((slot (car bucket))
				(ckey (car slot))
				(bucket-rest (cdr bucket)))
		   (if (funcall test key ckey) (cons (cons key val) (append bucket-rest acc))
			 (recur bucket-rest key val test (cons slot acc)))))))

(recur-defun* ptbl-get-from-bucket (bucket key test)
  "Find an association to BUCKET with KEY.  TEST defines key equality."
  (cond ((empty? bucket) nil)
		(t
		 (let* ((slot (car bucket))
				(ckey (car slot))
				(val (cdr slot))
				(bucket-rest (cdr bucket)))
		   (if (funcall test key ckey) val
			 (recur bucket-rest key test))))))

(defun bucket-keys (bucket)
  "Return all the keys in a BUCKET."
  (mapcar #'car bucket))

(defun bucket-values (bucket)
  "Return all the values in a BUCKET."
  (mapcar #'cdr bucket))


(defun ptbl-set (tbl key val)
  "Return a new persistent hash table which is like TBL except
that KEY is associated with VAL."
  (let* ((h (sxhash key))
		 (ix (mod h (ptbl-n-buckets tbl)))
		 (buckets (ptbl-buckets tbl))
		 (bucket (ra:list-ref buckets ix)))
	(ptbl-set-buckets tbl
					  (ra:list-set buckets ix (ptbl-add-to-bucket bucket key val (ptbl-test tbl))))))

(defun {} (maybe-ptbl &rest args)
  "Construct or augment a PTBL with the KEY/VAL pairs in ARGS.
If MAYBE-PTBL is not a PTBL, treat it as the first key and use an
empty persistent table."
  (if (not (ptbl? maybe-ptbl)) (apply #'{} the-empty-ptbl (cons maybe-ptbl args))
	(recur-let ((key/vals args)
				(ptbl maybe-ptbl))
			   (if (empty? key/vals) ptbl
				 (let ((key (car key/vals))
					   (val (cadr key/vals))
					   (rest (cddr key/vals)))
				   (recur rest
						  (ptbl-set ptbl key val)))))))

(defun ptbl-get (tbl key &optional or-value)
  "Retreive the association for KEY from the persistent hashtable
TBL.  Return OR-VALUE if no association exists, which defaults to
NIL."
  (let* ((h (sxhash key))
		 (ix (mod h (ptbl-n-buckets tbl)))
		 (buckets (ptbl-buckets tbl))
		 (bucket (ra:list-ref buckets ix)))
	(ptbl-get-from-bucket bucket key (ptbl-test tbl))))

(defun ptbl-keys (tbl)
  "Return a list of all keys in the persistent hash table TBL.
Order is unspecified."
  (recur-let ((buckets (ptbl-buckets tbl))
			  (keys '()))
			 (cond
			  ((ra:null? buckets) keys)
			  (t (recur
				  (ra:cdr buckets)
				  (append (bucket-keys (ra:car buckets)) keys))))))

(defun ptbl-values (tbl)
  "Return a list of all values in the persistent hash table TBL.
Order is unspecified."
  (recur-let ((buckets (ptbl-buckets tbl))
			  (vals '()))
			 (cond
			  ((ra:null? buckets) vals)
			  (t (recur
				  (ra:cdr buckets)
				  (append (bucket-values (ra:car buckets)) vals))))))

(defun ptbl->alist (tbl)
  "Return an association list with the same assocations as TBL."
  (recur-let ((keys (ptbl-keys tbl))
			  (pairs '()))
			 (if (empty? keys)
				 pairs
			   (recur (cdr keys)
					  (cons (cons (car keys)
								  (ptbl-get tbl (car keys))) pairs)))))

(defun ptbl->ppstring (tbl)
  "Produce a nice string representation of the persistent hash table TBL."
  (recur-let ((keys (ptbl-keys tbl))
			  (str "({} "))
			 (if (empty? keys)
				 str
			   (recur (cdr keys)
					  (concat str 
							  (format "%s%s %s%s%s"
									  (if (or (symbolp (car keys))
											  (listp (car keys))) "'" "")
									  (car keys)
									  (if (or (symbolp (ptbl-get tbl (car keys)))
											  (symbolp (ptbl-get tbl (car keys)))) "'" "")
									  (ptbl-get tbl (car keys))
									  (if (empty? (cdr keys)) ")" " ")))))))

(provide 'persistent-hash-tables)

