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

(defparser =stim-desc
  (current <- =number)
  =_
  (pulses <- =number)
  =_
  (freq <- =number)
  (=>maybe =_)
  (trial <- =maybe-letter->trial)
  (m-return (alist>> :trial trial :current current :pulses pulses :freq freq :type "stim")))


(provide 'parse-sombers-lab-files)