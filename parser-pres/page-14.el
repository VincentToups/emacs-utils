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

(defun -colon-then-trailing ()
  (parser-let* ((colon (-colon))
				(trailing (-trailing)))
			   (simple-parser-return trailing)))

(defun -colon ()
  (-matches ":"))

(defun -middle ()
  (parser-let* 
   ((colon (-not (-colon)))
	(contents (-zero-or-more #'anything)))
   (simple-parser-return (list :middle (reduce #'concat contents)))))


(setq tab (format "\t"))
(defun -whitespaces ()
  (-one-or-more (-or (-matches " ")
					 (-matches tab))))

(defun -middle-and-params ()
  (parser-let* ((middle (-middle))
				(params (-params)))
			   (list :continued (list middle params))))

(defun -params ()
  (parser-let*
   ((_ (-whitespaces))
	(trailing? (-maybe (-colon-then-trailing)))
	(rest (-maybe (-and-list 

(funcall (-params) " a b c:a")




;;;Controls Home   <<< .    1   2   3   4   5   6   7   8   9   10   11   12   13   14   
;;;         Index