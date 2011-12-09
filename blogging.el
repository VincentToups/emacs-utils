(defun literate-scheme->markdown (input-buffer output-buffer)
  (labels ((insertf* (&rest args) 
					 (with-current-buffer 
						 output-buffer (insert (apply #'format args))))
		   (text? (line)
				  (string= (substring line 0 (min 3 (length line))) ";;;")))
	(let ((lines (with-current-buffer
					 input-buffer
				   (buffer-all-lines))))
	  (loop for line in lines do
			(if (text? line)
				(insertf* "%s\n" (chomp (substring line 3)))
			  (insertf* "    %s\n" line))))))

(literate-scheme->markdown 
 (get-buffer "interpreter-01.rkt")
 (get-buffer "interpreter-01.md"))