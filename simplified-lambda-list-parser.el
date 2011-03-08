(require 'utils)
(require 'monad-parse)
(require 'functional)
(provide 'simplified-lambda-list-parser)

(defun not-lambda-list-sentinal (symbol)
  "Returns true for anything that isn't in '(&rest &optional &key)."
  (and (not (eq '&rest symbol))
	   (not (eq '&key symbol))
	   (not (eq '&optional symbol))))
(defun lambda-list-sentinal (symbol)
  "Returns true for anything that is in '(&rest &optional &key)."
  (or (eq '&rest symbol)
	  (eq '&optional symbol)
	  (eq '&key symbol)))
(defun lambda-list-tail-sentinal (symbol)
  "Returns true for either '&rest or '&key, which are mutually exclusive in a lambda list."
  (or (eq '&rest symbol)
	  (eq '&key symbol)))


(defun =not-lambda-list-sentinal ()
  "Return a parser which parses a single item which is not a lambda list sentinal."
  (=satisfies #'not-lambda-list-sentinal))
(defun =lambda-list-tail-sentinal ()
  "Return a parser which parses a single item which is a lambda list sentinal."
  (=satisfies #'lambda-list-tail-sentinal))

(defun =regular-args ()
  "Return a parser which gets the regular argument symbols of a lambda list,
that is, the symbols up to the first lambda list sentinal."
  (=let* [_ (zero-or-more (=not-lambda-list-sentinal))]
		 _))

(defun symbol-or-proper-pair (o)
  "Returns true for either a naked symbol, a list of only one symbol, or a list
with two elements, a symbol and a form, which is an arbitrary lisp expression."
  (or (not-lambda-list-sentinal o)
	  (and (listp o)
		   (symbolp (car o))
		   (<= (length o) 2))))

(defun =maybe-optional-arg ()
  "Returns a parser which parses a single argument representing a symbol and its
default value, or a symbol.  That is, x, (x) or (x 10), as examples."
  (=satisfies #'symbol-or-proper-pair))

(defun =optional-args ()
  "Returns a parser which parses optional arguments from a lambda list.  The 
parses returns a list of these args."
  (=let* [sentinal (=satisfies (par #'eq '&optional))
				   args (zero-or-more (=maybe-optional-arg))]
		 args))

(defun =key-args ()
  "Returns a parser which parses the arguments of the &key part of a lambda list, 
and returns the list of argument forms."
  (=let* [sentinal (=satisfies (par #'eq '&key))
				   args (zero-or-more (=maybe-optional-arg))]
		 args))

(defun =rest-arg ()
  "Returns a parser which parses the arguments of the &rest part of a lambda list,
and returns the symbol which will contain the tail of the passed in args."
  (=let* [sentinal (=satisfies (par #'eq '&rest))
				   arg (=not-lambda-list-sentinal)]
		 (if arg arg (error "Lambda list parser error - &rest needs a symbol to bind the rest to."))))

(defun =lambda-list-tail ()
  "Returns a parser which parses the tail of a lambda list, either an &key for, or an &rest form, 
but not both.  The context using this parser should check to see that this form exhausts the lambda list,
because not doing so indicates an error."
  (=let* [sentinal (=satisfies #'lambda-list-tail-sentinal)
				   part/s
				   (cond ((eq sentinal '&rest)
						  (=satisfies #'not-lambda-list-sentinal))
						 ((eq sentinal '&key)
						  (zero-or-more (=maybe-optional-arg))))]
		 (list (case sentinal
				 (&rest :rest)
				 ('&key  :key))
			   part/s)))

(defun =lambda-list ()
  "Returns a parser which parses a lambda list into an alist with :normal,
:optional, :key or :rest entries, containing the appropriate forms."
  (=let* [normals (zero-or-more (=regular-args))
				  optionals (=maybe (=optional-args))
				  tail (=maybe (=lambda-list-tail))]
		 (let ((table (alist>> :optional optionals :normal normals)))
		   (if tail
			   (cons tail table)
			 table))))

(defun simple-parse-lambda-list (lambda-list)
  "Parse the lambda list in LAMBDA-LIST and return a table of the lambda list information where
:normal holds the normal argument symbols
:optional holds the optional arguments, as either symbol or symbol/form pairs
:rest holds the symbol to bind the tail of the arguments to
and :key holds the key symbols or symbol/val pairs.

  Pairs are proper lists, rather than PAIRS in the strict sense.  Throws errors if the lambda
list is not parsable."
  (let* ((result-and-state (funcall (=lambda-list) (->in lambda-list)))
		 (result (car (car result-and-state)))
		 (remainder (input-as-list (cdr (car result-and-state)))))
	(if remainder (error "Can't figure out how to parse the lambda-list tail %S" remainder)
	  result)))














