;;; Example

;;; From RFC 1459, the IRC Chat Protocol Standards Docoument a Pseudo
;;; BNF description of an IRC Message.  Lets write a parser for this.

;;; IRC MESSAGE:
;; <message> ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <prefix> ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
;; <command> ::= <letter> { <letter> } | <number> <number> <number>
;; <SPACE> ::= ' ' { ' ' }
;; <params> ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
;; <middle> ::=
;;   <Any *non-empty* sequence of octets not 
;;   including SPACE or NUL or CR or LF, the 
;;   first of which may not be ':'>
;; <trailing> ::=
;;   <Any, possibly *empty*, sequence of octets 
;;   not including NUL or CR or LF>
;; <crlf> ::= CR LF



;;; We'll just assume that the line feed has been removed by a
;;; pre-parser that feeds us lines.

(defun -trailing ()
  (parser-let* ((trailing (-zero-or-more #'anything)))
			   (simple-parser-return
				(list :trailing (reduce #'concat trailing)))))

(defun -colon ()
  (-matches ":"))

(defun -colon-then-trailing ()
  (parser-let* ((colon (-colon))
				(trailing (-trailing)))
			   (simple-parser-return trailing)))



(setq tab (format "\t"))
(defun -whitespaces ()
  (-one-or-more (-or (-matches " ")
					 (-matches tab))))

(defun -middle ()
  (parser-let* 
   ((not-colon (-not (-colon)))
	(contents (-zero-or-more (-not-whitespace))))
   (simple-parser-return (list :middle (reduce #'concat contents)))))

(defun -space-middle ()
  (parser-let* 
   ((_ (-whitespaces))
	(middle (-middle)))
   (simple-parser-return middle)))

(defun -params ()
  (parser-let*
   ((params (-zero-or-more (-space-middle)))
	(_ (-whitespaces))
	(trailing (-maybe (-colon-then-trailing))))
   (simple-parser-return
	(cons (list :params 
				(mapcar #'cadr params))
		  (if trailing (list trailing)
			nil)))))

(defun -not-whitespace ()
  (-satisfies
   (lambda (x)
  (and (not (string= x " "))
       (not (string= x tab))))))

(defun -not-whitespaces ()
  (-zero-or-more (-not-whitespace)))

(lexical-let ((letters 
			   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
			  (numbers "1234567890")
			  (punctuation 
			   "~`!@#$%^&*()_+-={}[]|\\/<>,.:;'\"?"))
  (defun -letter ()
	(-satisfies
	 (lambda (x) 
	   (in (regexp-quote x) letters))))
  (defun -number ()
	(-satisfies 
	 (lambda (x)
	   (in (regexp-quote x) numbers))))
  (defun -punctuation ()
	(-satisfies 
	 (lambda (x)
	   (in (regexp-quote x) punctuation)))))

(defun -command ()
  (parser-let* 
   ((command (-or 
			  (-one-or-more (-letter))
			  (-n-of 3 (-number)))))
   (simple-parser-return
	(list :command (reduce #'concat command)))))

;;; We are going to cheat for the sake of brevity, and define prefix as:

(defun -prefix ()
  (parser-let* ((contents (-zero-or-more (-not-whitespace))))
			   (simple-parser-return (list :prefix (reduce #'concat contents)))))

;;; Putting it all together:

(defun -irc-message ()
  (parser-let* 
   ((_ (-colon))
	(prefix (-prefix))
	(_ (-whitespaces))
	(command (-command))
	(params&tail (-params)))
   (simple-parser-return
	(append (list prefix command) params&tail))))


(parsed-value (funcall (-irc-message) ":tod.com SEND a b c :rest"))

;;; WEEEEE


;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index