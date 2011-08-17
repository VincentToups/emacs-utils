(require 'monad-parse)
(require 'utils)

(defun expr->let-clause (expr)
  (cond 
   ((symbolp expr) (lambda

(defmacro extlet (expr &rest body)
  (cond
   ((symbolp expr) `(