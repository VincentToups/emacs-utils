;;; org-get-tags
(defun vorg-goto-heading (heading)
  
  (let ((old-point (point)))
	(goto-char (point-min))
	(loop with found = nil
		  while (not 
				 (and (org-on-heading-p)
					  (string= (org-get-heading)
							   heading)))
		  do
		  (if (org-on-heading-p)
			  (insert (org-get-heading)))
		  (if (and (last-line?)
				   (not found))
			  (progn 
				(goto-char old-point)
				(error "Heading %s not found" heading))
			(forward-line 1)))))
		
