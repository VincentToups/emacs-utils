(provide 'logging)
(require 'bigint)
(require 'sqlite)

(setq *logging-enabled* nil)
(setq *last-buffer-change* "")

(defun sawfish-like-current-time ()
  (let* ((days (time-to-number-of-days (current-time)))
		 (seconds (* (- days (floor days)) (* 24 60 60))))
	(cons (floor days) (floor seconds))))

(defun event-log-emacs-sql (raw other-data)
  (let* ((ct (sawfish-like-current-time))
		 (days (car ct))
		 (seconds (cdr ct))
		 (cts (current-time-string)))
	(print raw)
	(atomic-insert "/home/toups/Dropbox/event-log.db" 'events '(days seconds raw date extra) (list days seconds raw cts other-data))))

(defun event-logf (&rest args)
  (event-log-emacs-sql (apply #'format args) nil))
(defun event-log-alist (alist)
  (event-log-emacs-sql 
   (if (alist alist 'raw) (alist alist 'raw) "")
   alist))
(defun event-log-alist>> (&rest args)
  (event-log-alist (apply #'alist>> args)))

(defun log-buffer-entry-activity ()
  (if *logging-enabled*
	  (let ((name (buffer-name (current-buffer))))
		(if (not (string= name *last-buffer-change*))
			(progn 
			  (event-logf "Switched to %s in emacs" name)
			  (setq *last-buffer-change* name))))))

(comment 
(setq *logging-enabled* t)
)


(push #'log-buffer-entry-activity *after-select-window-hooks*)
