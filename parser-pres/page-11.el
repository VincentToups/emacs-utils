;;; Non-trivial Things

;;; Ok, what kinds of fun things can we do with this parser monad
;;; business?

;;; Well, imagine you wish to match either:
;;; ab
;;; bc or
;;; ca

;;; We can do this with a single expression using our monadic parser
;;; combinators.  Observe:


(defun parse-a|b|c (input)
  (unless (empty? input)
	(string-case (str-head input)
				 ("a" (pair :found-a (str-tail input)))
				 ("b" (pair :found-b (str-tail input)))
				 ("c" (pair :found-c (str-tail input))))))

(defun make-dependent-parser (last-result)
  (case last-result
	(:found-a #'parse-b)
	(:found-b #'parse-c)
	(:found-c #'parse-a)))

(setq triangle-parser 
	   (parser-let* ((first-char #'parse-a|b|c)
					 (second-char (make-dependent-parser first-char)))
					(simple-parser-return (cons first-char second-char))))

(funcall triangle-parser "ab")
(funcall triangle-parser "bc")
(funcall triangle-parser "ca")
(funcall triangle-parser "aa")
(funcall triangle-parser "cq")

(find-file-other-frame "~/work/art/haskell-curry-says.png")

;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index