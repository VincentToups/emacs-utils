(require 'monad-parse)

(lexical-let 
	((also-ok-in-ids 
	  (coerce "!@$%^&*_+-={}:.?/\|" 'list)))
  (defun other-id-char? (x) (in x also-ok-in-ids)))

(defun =other-id-char ()
  (=satisfies #'other-id-char?))

(funcall (=other-id-char) (->in "|"))

(defun =id-char ()
  (parser-plus-2 (alphanumeric) (=other-id-char)))

(funcall (=id-char) (->in "ab"))

(defun =lisp-symbol ()
  (=let* [_ (one-or-more (=id-char))]
		 (coerce _ 'string)))


(defun =numeric-sequence ()
  (=let* [_ (one-or-more (=digit-char))]
		 (coerce _ 'string)))

(defun =char->string (char)
  (=let* [_ (=char char)]
		 (coerce (list _) 'string)))

(defun =number ()
  (=let* [p1 (=numeric-sequence)
			 dot?  (zero-or-one (=char->string ?.))
			 p2 (zero-or-one (=numeric-sequence))]
		 (concat p1 dot? p2)))

(defun =escaped-quote ()
  (=string "\\\""))

(funcall (=escaped-quote) (->in "testbuffer.txt"))

(funcall (lisp-symbol) (->in "an"))

(funcall (=numeric-sequence) (->in "testbuffer.txt"))

(funcall (=char ?1) (->in "testbuffer.txt"))

(input-rest (->in "testbuffer.txt"))