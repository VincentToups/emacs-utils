(provide 'monad-transformers)
(require 'monads)
(require 'utils)

(defun sequence-t (inner-monad)
  (alist 
   :m-bind (lambda (v f) 
			 (with-monad inner-monad 
						 (mapcat 
