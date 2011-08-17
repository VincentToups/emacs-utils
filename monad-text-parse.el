(require 'utils)
(require 'monads)
(require 'recur)

(defmacro when/not-empty (val &rest body)
  (with-gensyms 
   (id)
   `(let ((,id ,val))
	  (when (and ,id (not (empty? ,id)))
		,@body)))) 

(defun match-string< (str)
  (let ((str (format "%s" str)))
	(enclose 
	 (str)
	 (lambda (input)
	   (when input
		 (let* ((n (length str))
				(k (min (length input) n))
				(test (substring input 0 k))
				(rest (substring input k)))
		   (if (string= test str)
			   (list (cons str rest)) 
			 nil)))))))

(defun ->parser (thing)
  (if (functionp thing) thing
	(match-string< thing)))

(defun text-parse-bind (parser* parser-producer)
  (let ((parser* (->parser parser*)))
	(enclose 
	 (parser* parser-producer)
	 (lambda (input)
	   (when input
		 (recur-let 
 		  ((results (funcall parser* input))
		   (output '()))
		  (cond ((empty? results) output)
				(t
				 (printf "results %s" results)
				 (let* ((first-pair (car results))
						(rest-pairs (cdr results))
						(new-parser (->parser (funcall parser-producer (car first-pair))))
						(new-results (funcall new-parser (cdr first-pair))))
				   (recur rest-pairs (append output new-results)))))))))))

(defun text-parse-return (item)
  (enclose 
   (item)
   (lambda (input)
	 (list (cons item input)))))

(defun string-head (s)
  (if (empty? s) s
	(substring s 0 1)))

(defun string-tail (s)
  (if (empty? s) "" 
	(substring s 1)))

(defun /item/ (input)
  (when/not-empty input
				  (list (cons (string-head input) (string-tail input)))))

(setq monad-text-parse 
	  (tbl! :m-return #'text-parse-return
			:m-bind #'text-parse-bind))

(funcall (lexical-mlet monad-text-parse
			  ((a "a")
			   (b "b"))
			  (m-return (list a b))) "ab")