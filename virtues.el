(require 'defn)

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


; (setq sums (slurp-virtue-file "/home/toups/Dropbox/gtd/virtues/07_14_2009.org"))
; (average-score sums "humility")
