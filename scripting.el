(require 'utils)
(require 'functional)
(require 'with-stack)
(require 'stack-words)

(defun dircat (&rest args)
  (join (filter (/| (not (string= "" %1))) (flatten 
											(mapcar 
											 (clambdar #'split-string "/")
											 args))) "/"))




(setq *file-info-alist* '(("%a" (:access-rights-in-octal (lambda (x) x)))
						  ("%A" (:access-rights-in-human-readable-form (lambda (x) x)))
						  ("%b" (:number-of-blocks-allocated-see-%b string-to-number))
						  ("%B" (:the-size-in-bytes-of-each-block-reported-by-%b string-to-number))
						  ("%d" (:device-number-in-decimal string-to-number))
						  ("%D" (:device-number-in-hex (lambda (x) x)))
						  ("%f" (:raw-mode-in-hex (lambda (x) x)))
						  ("%F" (:file-type (lambda (x) x)))
						  ("%g" (:group-id-of-owner (lambda (x) x)))
						  ("%G" (:group-name-of-owner (lambda (x) x)))
						  ("%h" (:number-of-hard-links (lambda (x) x)))
						  ("%i" (:inode-number (lambda (x) x)))
						  ("%n" (:file-name (lambda (x) x)))
						  ("%N" (:quoted-file-name-with-dereference-if-symbolic-link (lambda (x) x)))
						  ("%o" (:i/o-block-size (lambda (x) x)))
						  ("%s" (:total-size-in-bytes string-to-number))
						  ("%t" (:major-device-type-in-hex (lambda (x) x)))
						  ("%T" (:minor-device-type-in-hex (lambda (x) x)))
						  ("%u" (:user-id-of-owner (lambda (x) x)))
						  ("%U" (:user-name-of-owner (lambda (x) x)))
						  ("%x" (:time-of-last-access (lambda (x) x)))
						  ("%X" (:time-of-last-access-as-seconds-since-epoch string-to-number))
						  ("%y" (:time-of-last-modification (lambda (x) x)))
						  ("%Y" (:time-of-last-modification-as-seconds-since-epoch string-to-number))
						  ("%z" (:time-of-last-change (lambda (x) x)))
						  ("%Z" (:time-of-last-change-as-seconds-since-epoch string-to-number))))
(setq *file-info-alist-keys*
	  (mapcar (comp #'car #'cadr) *file-info-alist*))


(setq *simple-file-info*
	  `(("%n" (:name (lambda (x) x)))
		("%Y" (:last-change string-to-number))
		("%s" (:size string-to-number))))

(defun* file-info (file &optional (format-alist *simple-file-info*))
  (let* ((format (concat "--format=\"" (join (mapcar #'car format-alist) " ") "\""))
		 (parts (split-string 
				 (car (shf "stat -t %s %s" format file)) " ")))
	(zip (mapcar (comp #'car #'cadr) format-alist)
		 (mapcar* (lambda (part alist-part)
					(let ((f (cadr (cadr alist-part))))
					  (funcall f part)))
				  parts format-alist))))
(defun file-loc (filename)
  (||| lisp-val: (split-string filename "/") reverse cdr reverse "/" 2>join))

(defun file-name (filename)
  (||| lisp-val: (split-string filename "/") reverse car))

(defun file-name-flatten-with (filename rep)
  (join (filter 
		 (lambda (x) (not (or (eq x nil) (string= x "")
							  (string= x " ") (string= x ".")
							  (string= x ".."))))
		 (split-string filename "/") ) rep))

(defun* pluck (filename &optional (n-in 0))
  (let* ((loc-parts (reverse (split-string (file-loc filename) "/")))
		 (name (file-name filename))
		 (partial-stem (reverse (ix: loc-parts (range n-in)))))
	(concat (apply #'dircat partial-stem) (if (> (length partial-stem) 0) "/" "")  name)))

(defun* clip (filename &optional (n-at 0))
  (let* ((loc-parts (split-string (file-loc filename) "/"))
		 (name (file-name filename))
		 (partial-stem (ix: loc-parts (range n-at end+))))
	(concat (apply #'dircat partial-stem) (if (> (length partial-stem) 0) "/" "")  name)))

(defun rep-underscores (str with)
  (replace-string-in-string " " with str))

(defun remove-string (string string-to-remove)
  (replace-string-in-string string-to-remove "" string))

(defun rename-file-if-different (file new-name &rest args)
  (if (not (string= file new-name)) 
	  (apply #'rename-file file new-name args)
	nil))

(defun* remove-spaces (filename &optional (rep "_"))
  (rename-file-if-different filename 
							(replace-regexp-in-string
							 (rx whitespace)
							 rep
							 filename)))

(defmacro with-working-directory (dir &rest body)
  (with-gensyms (hold-dir%)
				`(let ((,hold-dir% (wd)))
				   (unwind-protect
					   (progn 
						 (cd ,dir)
						 ,@body)
					 (cd ,hold-dir%)))))

(defun* remove-spaces-in-dir (directory)
  (with-working-directory directory
						  (loop for f in (sh "ls -1")
								do
								(remove-spaces f))))

(defun remove-spaces-in-tree (root)
  (with-working-directory root
						  (remove-spaces-in-dir (wd))
						  (let ((sub-dirs
								 (filter #'directoryp 
										 (sh "ls -1"))))
							(loop for s in sub-dirs do
								  (print s)
								  (remove-spaces-in-tree s)))))

(provide 'scripting)

