(require 'utils)
(require 'recur)

(defstruct quantity value error units)
(defstruct qunit name type conversions)

(defvar *unit-types* '())
(defvar *units* '())

(defun find-unit-with-name (name)
  (recur-let 
   ((lst *units*))
   (cond
	((empty? lst) nil)
	((eq (qunit-name (car lst) 'name)) (car lst))
	(t (recur (cdr lst))))))

(defmacro qunit (spec)
  (if (symbolp spec) `(find-unit-with-name ',spec)
	spec))

(defmacro declare-unit (unit-name type &rest conversions)
  `(progn 
	 (push 
	  (make-qunit :name ',unit-name :type ',type :conversions (list ,@conversions)) *units*)
	 (setq *unit-types* 
		   (alist-cons *unit-types* ',type (qunit ',unit-name)))))

(declare-unit 'grams 'mass)