;;; Atomic Parsers!

;;; You could imagine a ton of "parse-_" style parsers, but turns out
;;; there are even simpler parsers:

(defun anything (input) ; aka "item"
  (unless (empty? input)
	(pair (str-head input) (str-tail input))))

;;; And one very important _parameterized_ parser:

(defun simple-parser-return (val)
  (lexical-let ((val val))
	(lambda (input)
	  (pair val input))))

(defun nil-parser (input)
  nil)


;;; This takes a value and returns a parser which "returns" that
;;; value, without changing the input.  If you wanted to insert a
;;; value into your parsers for some reason, this is the function
;;; you'd use.  
;;;
;;; It, too, will be of importance later.

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index