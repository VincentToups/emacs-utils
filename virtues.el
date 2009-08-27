(require 'defn)
(require 'org)

(defvar all-virtues (reverse (list 
					 "humility"
					 "chastity"
					 "tranquility"
					 "cleanliness"
					 "moderation"
					 "justice"
					 "sincerity"
					 "industry"
					 "frugality"
					 "resolution"
					 "order"
					 "silence"
					 "temperance")))

(defn default-virtue-sums []
  (loop with sums = (tbl!) 
		for v in
		(list 
		 "humility"
		 "chastity"
		 "tranquility"
		 "cleanliness"
		 "moderation"
		 "justice"
		 "sincerity"
		 "industry"
		 "frugality"
		 "resolution"
		 "order"
		 "silence"
		 "temperance")
		do
		(tbl! sums v 0)
		(tbl! sums (concat v "-count") 0)
		finally
		(return sums)))

(defn chomp-properties [str]
  (chomp (substring-no-properties str)))

(defn slurp-virtue-file 
  ([filename sums]
   (dlet [buf (find-file-noselect filename)]
	 (with-current-buffer buf
	   (loop for i from 2 to 14 do 
			 (goto-line i)
			 (dlet [virtue (chomp-properties (org-table-get-field 1))
						   score  (string-to-number (chomp-properties (org-table-get-field 2)))]
			   (tbl! sums virtue (+ score (tbl sums virtue)))
			   (tbl! sums (concat virtue "-count") (+ 1 (tbl sums (concat virtue "-count")))))))
	 (kill-buffer buf)
	 sums))
  ([filename]
   (slurp-virtue-file filename (default-virtue-sums))))

(defn average-score [sums virtue]
  (round (/ (float (tbl sums virtue))
			(float (tbl sums (concat virtue "-count"))))))

(defn average-scores [sums]
  (loop with averages = (tbl!)
		for key in all-virtues do
		(tbl! averages key (average-score sums key))
		finally (return averages)))

										;(keyshash sums)

										; (setq sums (slurp-virtue-file "/home/toups/Dropbox/gtd/virtues/07_14_2009.org"))
										; (average-scores sums)



(defn n-spaces [n]
  (make-string n (car (coerce " " 'list))))

(defn print-virtue-table [averages]
  (let* ((max-len (apply #'max (mapcar #'length all-virtues)))
		 (padded-virtues (mapcar 
						  (fn [v] (concat v (n-spaces (- max-len (length v))) ": "))
						  all-virtues)))
	(loop for vp in padded-virtues 
		  and
		  virt in all-virtues do
		  (insertf "%s %s\n" vp (make-string (tbl averages virt) ?*)))))

(defvar *virtues-directory* "~/Dropbox/gtd/virtues")

(defn org-file? [filename]
  (dlet [len (length filename)]
	(if ($ len < 4) nil
	  (dlet [last-four (substring-no-properties filename (- len 4) len)]
		(string= last-four ".org")))))

(defn get-files []
  (mapcar (fn [f] (concat *virtues-directory* "/" f)) (filter #'org-file? (directory-files *virtues-directory*))))

(defn find-averages-from-all-files []
  (dlet [files (get-files)
			   sums (foldl (fn [it ac]
							   (slurp-virtue-file it ac))
						   (slurp-virtue-file (car files))
						   (cdr files))]
	(average-scores sums)))

(defun show-averages-buffer ()
  (interactive)
  (dlet [b (get-buffer-create "*virtues-average-buffer*")]
	(with-current-buffer b 
	  (clear-buffer)
	  (print-virtue-table (find-averages-from-all-files)))))

(provide 'virtues)






