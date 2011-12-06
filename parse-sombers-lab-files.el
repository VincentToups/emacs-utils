(require 'better-monad-parse)
(require 'scripting)

(defparser =units 
  (=>string 
   "M"
   "uM"
   "nM"
   "L"
   "mL"
   "nL"))

(defun char->trial (a)
  (if (stringp a)
	  (char->trial (car (coerce a 'list)))
	(- a ?a)))

(defparser =maybe-letter->trial
  (c <- (=>maybe =alpha-lower))
  (m-return
   (if c (char->trial c)
	 0)))

(defparser =_ (=>string "_"))

(defparser =rotenone 
  (=>stringi "rotenone"
			 "rot"))

(defparser =saline
  (=>stringi "saline" "sal"))

(defparser =/ (=>string "/"))

(defparser (=>pluck n)
  (in <- =input)
  (lexical-let ((new (pluck in n)))
	(parser
	 (=>set-input new)
	 (m-return new))))


(provide 'parse-sombers-lab-files)