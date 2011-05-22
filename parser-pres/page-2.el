;;; Parsers

;;; The whole point here is to enable us to build complex parsers out
;;; of simple ones.  
;;; 
;;; A simple parser is a function which takes an input and returns either:
;;;  * nil, if the parser doesn't see what it wants
;;;  * or a pair ( produced-value . left-over-input )

;;; eg:

(defun str-head (str)
  (substring str 0 1))
(defun str-tail (str)
  (substring str 1))
(defun pair (a b)
  (cons a b))
(defun parsed-value (pair)
  (car pair))
(defun parsed-leftover (pair)
  (cdr pair))


(defun parse-a (input)
  "A very simple parser - parses 'a' or nothing."
  (unless (empty? input)
	(if (string= "a" (str-head input))
		(pair :found-a (str-tail input))
	  nil)))

(parse-a "abracadabra")
(parse-a "dogs of war")
   
;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index