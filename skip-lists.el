(provide 'skip-lists)
(require 'cl)


(defun skip-list-level (p max-depth)
  (loop with depth = 1 
		while (and (< p (random* 1.0))
				   (< depth max-depth)) do
		(setq depth (+ 1 depth))
		finally (return depth)))
(
defun* node (val &optional (p (/ 1 2.0)) (max-depth 16))
  (cons val (list->vector (range (+ 1 (skip-list-level p max-depth))))))

(defun node-width (node)
  (elt (cdr node) 0))

(defun node-depth (node)
  (- (length (cdr node) 1)))

(defun set-node-width (node to) (setf (elt (cdr node) 0) to))

(defun set-node-link (node depth to) 


(defun node-skip-cdr (node i)
  (let* ((cdr-part (cdr node))
		 (n-links (- (length cdr-part) 1)))
	(if (> i >= n-links) (error "Requested a node-link deeper than the available depth.")
	  (elt cdr-part (+ i 1)))))


  