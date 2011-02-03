(require 'monad-parse)
(require 'cl)
(provide 'lisp-parser)

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
		 (read (coerce _ 'string))))


(defun =numeric-sequence ()
  (=let* [_ (one-or-more (=digit-char))]
		 (coerce _ 'string)))

(defun =int ()
  (=let* [sign (zero-or-one (=or (=char->string ?-)
								 (=char->string ?+)))
			   s (=numeric-sequence)]
		 (string-to-number (concat sign s))))


(defun =char->string (char)
  (=let* [_ (=char char)]
		 (coerce (list _) 'string)))

(defun =number ()
  (=or (=float) (=int)))

(defun =float ()
  (=let* [sign (zero-or-one (=or (=char->string ?+) (=char->string ?-)))
			   p1 (zero-or-more (=digit-char))
			   dot (=char ?.)
			   p2 (one-or-more (=digit-char))]
		 (string-to-number (concat sign p1 "." p2))))


(defunc =escaped-quote ()
  (=let* [_ (=string "\\\"")]
		 (if _ ?\" nil)))

(setq space  ?\s)

(defun =spaces ()
  (zero-or-more (=char ?\s)))
(defun =space ()
  (=char ?\s))

(defun =lisp-atom ()
  (=let* [_ (=spaces)
			atom (=or (=number)
					  (=lisp-string)
					  (=lisp-symbol))
			]
		 atom))

(defunc =lisp-string ()
  (=let* [_ (=char ?\")
			contents (zero-or-more (=or 
									(=escaped-quote)
									(=satisfies
									 (lex-lambda (c) (!= c ?\")))))
			_ (=char ?\")]
		 (coerce (flatten contents) 'string)))



