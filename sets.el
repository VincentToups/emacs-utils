(provide 'sets)
(require 'utils)
(require 'cl)

(defun* make-set (members &optional (pred #'equal))
  (alist>> :values (unique members pred)
		   :pred pred))

(defun predicate-of-set (set)
  (alist set :pred))

(defun values-of-set (set)
  (alist set :values))

(defun check-set-compat (set1 set2)
  (if (not (equal 
			(predicate-of-set set1)
			(predicate-of-set set2)))
	  (error "Can't operate on sets with distinct predicates.")
	t))

(defun* set-union (set1 set2)
  (check-set-compat set1 set2)
  (make-set (unique (append (values-of-set set1)
							(values-of-set set2)) (predicate-of-set set1))
			(predicate-of-set set1)))

(defun in-set (object set)
  ($ object in (values-of-set set) (predicate-of-set set)))
(defun add-to-set (set object)
  (if (in-set object set) set
	(alist-cons set :values object)))

(defun set-intersection (set1 set2)
  (check-set-compat set1 set2)
  (let ((vals (values-of-set (set-union set1 set2))))
	(make-set
	 (filter
	  (lambda (item) 
		(and ($ item in-set set1)
			 ($ item in-set set2)))
	  vals)
	 (predicate-of-set set1))))

(defun set-difference (set1 set2)
  (check-set-compat set1 set2)
  (let ((v1 (values-of-set set1))
		(v2 (values-of-set set2)))
	(filter 
	 (lambda (i1)
	   (not ($ i1 in-set set2)))
	 v1)))

(defun set-count (set)
  (length (values-of-set set)))

(defun set-equality (set1 set2)
  (check-set-compat set1 set2)
  (= (set-count set1)
	 (set-count (set-union set1 set2))))
