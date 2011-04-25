(require 'monads)

(setq *go-button* nil)
(setq *reset-button* nil)
(defun namer-go (&optional button)
  (with-current-buffer "*namer*"
	(save-excursion 
	  (goto-char (point-min))
	  (let ((name-parts nil))
		(while (= 0 (forward-line))
		  (let* ((line-contents (buffer-subline))
				 (split-string (split-string line-contents "/")))
			(mlet monad-maybe^i 
				  ((_ (= (length split-string) 2))
				   (_ (not (string= (car split-string) "<property>")))
				   (_ (not (string= (cadr split-string) "<value>")))
				   (prop (car split-string))
				   (val (cadr split-string)))
				  (setq name-parts (append name-parts (list prop val))))))
		(put-string-on-kill-ring (join name-parts "="))
		(goto-char (point-max))
		(insertf "\n%s" (join name-parts "="))))))

(defun make-namer-buffer (&optional button)
  (interactive)
  (let ((buf (get-buffer-create "*namer*")))
	(with-current-buffer buf
	  (delete-region (point-min)
					 (point-max))
	  (setq *go-button* 
			(insert-button "Create Filename" 'action
						   #'namer-go))
	  (insert "\n")
	  (loop for i from 1 to 5 do
			(insertf "<property>/<value>\n"))
	  (setq *reset-button* 
			(insert-button 
			 "Reset" 
			 'action
			 #'make-namer-buffer)))))

(make-namer-buffer)

