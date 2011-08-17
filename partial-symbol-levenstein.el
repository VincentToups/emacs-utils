(require 'levenshtein)
(require 'functional)
(require 'utils)

(defun partial-levenshtein (s1 s2 delim &optional absent-identical-p)
  (let* ((parts1 (split-string s1 delim t))
		 (parts2 (split-string s2 delim t))
		 (sorted-by-len (sort* (list parts1 parts2) (decorate-all #'> #'length)))
		 (parts1 (car sorted-by-len))
		 (parts2 (cadr sorted-by-len))
		 (n-parts-2 (length parts2))
		 (parts2 
		  (loop for i from 0 below (length parts1) 
				collect
				(if (< i n-parts-2) (elt parts2 i)
				  (if absent-identical-p (elt parts1 i) "")))))
	(reduce #'+ (mapcar* #'levenshtein-distance parts1 parts2))))

(defun partial-symbol-levenshtein (s1 s2)
  (partial-levenshtein (format "%s" s1) (format "%s" s2) "-"))


(substring (md5 "skype+ru5tyb!kes") 0 20)"
"

(+ 5 25 7)0

