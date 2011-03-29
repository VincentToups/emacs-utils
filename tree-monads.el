(require 'utils)
(require 'monads)
(require 'functional)

(defun* btcons (val &optional (left nil) (right nil))
  (cons val
		(vector left right)))

(defun btree? (val)
  (and (consp val)
	   (vectorp (cdr val))))

(defun btleft (node)
  (first (cdr node)))

(defun btright (node)
  (second (cdr node)))

(defun btleaf? (val)
  (and (btree? val)
	   (eq (btleft val) nil)
	   (eq (btright val) nil)))

(defun btnode? (val)
  (not (btleaf? val)))

(defun btnode-val (val)
  (car val))

(defun left-nil? (node)
  (not (btleft node)))

(defun right-nil? (node)
  (not (btright node)))

(defun graft-left (tree sub-tree)
  (cond
   ((not tree) sub-tree)
   ((not sub-tree) tree)
   ((left-nil? tree) (btcons 
					  (btnode-val tree)
					  sub-tree
					  (btright tree)))
   (t (btcons (btnode-val tree)
			  (graft-left (btleft tree) sub-tree)
			  (btright tree)))))

(defun graft-right (tree sub-tree)
  (cond
   ((not tree) sub-tree)
   ((not sub-tree) tree)
   ((right-nil? tree) (btcons 
					  (btnode-val tree)
					  (btleft tree)
					  sub-tree
					  ))
   (t (btcons (btnode-val tree)
			  (btleft tree)
			  (graft-right (btright tree) sub-tree)
			  ))))

(graft-right (graft-left (btcons 10) (btcons 9)) (btcons 11))

(defun bt-bind (v f)
  (cond
   ((eq nil v) nil)
   ((or (btleaf? v) 
		(btnode? v)) 
	(let ((res (funcall f (btnode-val v))))
	  (graft-right (graft-left res (bt-bind (btleft v) f))
				   (bt-bind (btright v) f))))
   ))

(bt-bind (btcons 10 (btcons 9) (btcons 11))
		 (lambda (v) (btcons (+ v 1))))

(setq monad-btree 
	  (tbl! :m-return #'btcons
			:m-bind 
			#'bt-bind))



