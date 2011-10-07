(require 'utils)
(require 'recur)

(defstruct unit-atom name type conversions)
(defstruct unit-comp num den)

(defun u*2 (u1 u2)
  (make-unit-comp 
   :num
   (cond 
	((and (unit-atom? u1) 
		  (unit-atom? u2))
	 (list u1 u2))
	((and (unit-atom? u1)
		  (unit-comp? u2))
	 (cons u1 (unit-comp-num u2)))
	((and (unit-comp? u1)
		  (unit-atom? u2))
	 (suffix (unit-comp-num u1) u2))
	((and (unit-comp? u1)
		  (unit-comp? u2))
	 (append (unit-comp-num u1)
			 (unit-comp-num u2))))
   :den
   (cond 
	((and (unit-atom? u1) 
		  (unit-atom? u2))
	 (list u1 u2))
	((and (unit-atom? u1)
		  (unit-comp? u2))
	 (cons u1 (unit-comp-den u2)))
	((and (unit-comp? u1)
		  (unit-atom? u2))
	 (suffix (unit-comp-den u1) u2))
	((and (unit-comp? u1)
		  (unit-comp? u2))
	 (append (unit-comp-den u1)
			 (unit-comp-den u2))))))


(defun u* (&rest us)
  (reduce #'u*2 us))

(defun u-invert (u)
  (if (unit-atom? u)
	  (make-unit-comp
	   :num '()
	   :den (list u))
	(make-unit-comp 
	 :num (unit-comp-den u)
	 :den (unit-comp-num u))))

(defun u/ (u1 &rest us)
  (apply #'u* u1 (mapcar #'u-invert us)))

