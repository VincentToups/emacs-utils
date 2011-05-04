(require 'utils)
(require 'recur)

(defun op> (op1 op2 op-table)
  (> (tbl op-table op1)
	 (tbl op-table op2)))






