(provide 'monad-transformers)
(require 'monads)
(require 'utils)

(defun seq-t (inner-monad)
  (lexical-let ((inner-monad inner-monad))
	(tbl! 
	 :m-return
	 (lambda (val)
	   (call-return inner-monad (list val)))
	 :m-bind
	 (lex-lambda (mv mf)
				 (call-bind inner-monad
							mv
							(lambda (seq)
							  (reduce (m-lift-into 2 #'swons inner-monad)
									  (reverse (mapcat mf seq))
									  :initial-value (call-return inner-monad nil))))))))

(defun state-t (inner-monad)
  "Produce a monad by transforming INNER-MONAD with STATE-MONAD properties."
  (lexical-let ((inner-monad inner-monad))
	(tbl!
	 :m-return 
	 (lex-lambda (val)
				 (call-return inner-monad (lambda (state) (list val state))))
	 :m-bind 
	 (lex-lambda (mv mf)
				 (lex-lambda (state)
							 (call-bind inner-monad 
										mv
										(lambda (state-fun)
										  (let-seq (val new-state)
												   (funcall state-fun state)
												   (let ((new-f (funcall mf val)))
													 (funcall new-f new-state))))))))))

