(require 'utils)
(require 'monad-parse)
(require 'cl)
(require 'multi-methods)
(require 'with-stack)
(require 'stack-words)

(defun =trailing ()
  (zero-or-more (=satisfies (always t))))

(defun =trailing->string ()
  (=simple-let* ((tail (=trailing)))
				(coerce tail 'string)))

(defun =spaces-colon-trailing ()
  (=simple-let* ((_ (=maybe (=spaces)))
				 (_ (=colon))
				 (contents (=trailing)))
				(coerce contents 'string)))

(defun =space ()
  (=satisfies (lambda (x) (= ?\s x))))

(defun =spaces ()
  (one-or-more (=space)))

(defun =non-space ()
  (=satisfies (lambda (x) (not (= ?\s x)))))

(defun =zero-or-more-non-spaces ()
  (zero-or-more (=non-space)))

(defun =middle ()
  (=simple-let* ((non-colon (=non-colon))
				 (rest (zero-or-more (=non-space))))
				(coerce (cons non-colon rest) 'string)))

(defun =spaces-middle ()
  (=simple-let* ((_ (=maybe (=spaces)))
				 (mid (=middle)))
				(coerce mid 'string)))

(defun =colon ()
  (=satisfies (lambda (x) (= ?: x))))

(defun =non-colon ()
  (=satisfies (lambda (x) (not (= ?: x)))))

(defun =params ()
  (=simple-let* ((middles (zero-or-more (=spaces-middle)))
				 (tail (=maybe (=spaces-colon-trailing))))
				(alist>> :params middles :tail tail)))
(defun =command ()
  (=simple-let* ((first (=letter))
				 (rest (zero-or-more (alphanumeric))))
				(coerce (cons first rest) 'string)))

(defun =prefix ()
  (=simple-let* ((colon (=colon))
				 (contents (=zero-or-more-non-spaces)))
				(coerce contents 'string)))

(defun =irc-message ()
  (=simple-let* ((prefix (=maybe (=prefix)))
				 (_ (=maybe (=spaces)))
				 (command (=command))
				 (params&tail (=params)))
				(alist>> :prefix prefix
						 :command command
						 :params (alist params&tail :params)
						 :tail (alist params&tail :tail))))

(defvar *cb-messages* nil "Collected Messages from the bot buffer.")
(defun* cb-connect (host &key 
						 (port 6667)
						 (nick "cb-bot")
						 (channel "#test-channel"))
  (let ((con-buf (network-connection host port)))
	(comint-send-strings con-buf
						 (format "NICK %s" nick)
						 (format "USER %s 0 * :bot" nick)
						 )
	(run-with-timer 3 nil 
					(lexical-let ((channel channel)
								  (con-buf con-buf))
					  (lambda () (comint-send-strings con-buf (format "JOIN %s" channel)))))
	con-buf))

(defun cb-collect-messages ()
  (with-current-buffer *cb-connection*
	(let ((messages (split-string (buffer-substring (point-min) (point-max)) "\n")))
	  (delete-region (point-min) (point-max))
	  (setq *cb-messages* (append *cb-messages* messages))
	  *cb-messages*)))

(defvar *current-channel* nil "Dynamically bound current channel.")

(defun cb-pong (ping-contents)
  (let ((from (replace-regexp-in-string (rxq "PING :") "" ping-contents)))
	(comint-send-strings *cb-connection* (format "PONG %s" from))))

(defun* cb-message (message &optional (channel *current-channel*))
  (comint-send-strings *cb-connection* (format "PRIVMSG %s :%s" channel message)))

(defun get-privmsg-channel (msg)
  (elt (split-string msg " ") 2))

(defmulti cb-handle-message :command "Multimethod for handling IRC messages.")
(defunmethod-default cb-handle-message (parsed-message) nil)
(defunmethod cb-handle-message "PING" (parsed-message) 
  (comint-send-strings *cb-connection* (format "PONG %s" (alist parsed-message :prefix))))

(defunmethod cb-handle-message "PRIVMSG" (parsed-message)
  (let ((*current-channel* (car (alist parsed-message :params))))
	(cb-handle-privmsg-content (parse-string (=sub-parse-msg) (alist parsed-message :tail)))))

(defmulti cb-handle-privmsg-content :sub-command-type "Sub-handler for communication via PRIVMSG.")
(defunmethod-default cb-handle-privmsg-content (parsed-message) nil)
(defunmethod cb-handle-privmsg-content "echo" (parsed-message)
  (cb-message (format "echo: %s" (alist parsed-message :tail))))
(defunmethod cb-handle-privmsg-content "|||" (parsed-message)
  (let ((result nil))
	(unwind-protect 
		(progn 
          (setq result (eval `(||| ,@(read (concat "(" (alist parsed-message :tail) ")"))))))
	  (if result (cb-message result)
		(cb-message (format "Failed to execute command \"%s\" with error \"%s\"."
							(chomp (alist parsed-message :tail))
							*last-error*))))))


(defunmethod cb-handle-privmsg-content "" (parsed-message)
  (get-method-funcall 'cb-handle-privmsg-content "|||" parsed-message))
(defunmethod cb-handle-privmsg-content "e" (parsed-message)
  (cb-message (eval `(||| ,@(let* ((lst (read (concat "(" (alist parsed-message :tail) ")")))
								   (head (car lst))
							       (tail (cdr lst)))
							  (suffix tail head))))))



(defun =non-comma ()
  (=satisfies (lambda (x) (not (= ?, x)))))

(defun =comma ()
  (=satisfies (lambda (x) (= ?, x))))

(defun =chatter ()
  (=simple-let* ((non-comma (=non-comma))
				 (rest (zero-or-more (=satisfies (always t)))))
				(alist>> :sub-command-type "chatter" :tail (coerce (cons non-comma rest) 'string))))

(defun =sub-command-type ()
  (=simple-let* ((cmd (zero-or-more (=non-colon))))
				(coerce cmd 'string)))

(defun =sub-command ()
  (=simple-let* ((comma (=comma))
				 (sub-command-type 
				  (=sub-command-type))
				 (tail (=spaces-colon-trailing)))
				(alist>> :tail tail 
						 :sub-command-type sub-command-type)))

(defun =sub-parse-msg ()
  (=simple-let* ((res (=or (=chatter)
						   (=sub-command))))
				res))

(defun process-messages ()
  (loop while *cb-messages* do
		(let* ((msg (pop *cb-messages*)))
		  (let-if parsed (parse-string (=irc-message) msg)
				  (progn 
					(cb-handle-message parsed))
				  nil))))

(defun cb-cycle ()
  (if *cb-connection*
	  (progn (cb-collect-messages)
			 (process-messages))))

(cb-cycle)

*cb-messages*

(setq *cb-cyble-timer* (run-with-timer 1 1 
									   #'cb-cycle))

(cancel-timer *cb-cyble-timer*)

(||| word: hello "hello" end:)

(cb-collect-messages)
(process-messages)
*cb-messages*

(cb-pong)

(setq *cb-connection* (cb-connect "localhost"))
(setq *cb-connection* (cb-connect "irc.freenode.net"))

(comint-send-strings *cb-connection* 
					 "JOIN #cloud-test-channel")
(comint-send-strings *cb-connection* "PRIVMSG #test-channel :this is a response from me")

(parse-string (=irc-message) ":toups!user@127.0.0.1 PRIVMSG #test-channel :hello")
(parse-string (=command) "PRIVMSG")

(parse-string (=spaces) "S")