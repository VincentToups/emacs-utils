(require 'with-stack)
(setq *units-map* (alist>>))
(flet ((drop-last (x) 
				  (let* ((s (format "%s" x))
						 (n (length s)))
					(intern (substring s 0 (- n 1))))))
  (loop for 
		name in 
		'(base- deca-	hecto-	kilo-	mega-	giga-	tera-	peta-	exa-	zetta-	yotta-) and
		sub-name in
		'(base- 	deci-	centi-	milli-	micro-	nano-	pico-	femto-	atto-	zepto-	yocto-) and
		num in 
		'(0	1	2	3	6	9	12	15	18	21	24) do
		(eval `(defun ,(drop-last (internf "from-%s" name)) (v) (* v ,(expt 10.0 num))))
		(eval `(defun ,(drop-last (internf "in-%s" name)) (v) (/ v ,(expt 10.0 num))))
		(eval `(defun ,(drop-last (internf "from-%s" sub-name)) (v) (* v ,(expt 10.0 (- num)))))
		(eval `(defun ,(drop-last (internf "in-%s" sub-name)) (v) (/ v ,(expt 10.0 (- num)))))

		(eval `(univalent-stack-words ,(drop-last (internf "from-%s" name))
									  ,(drop-last (internf "in-%s" name))
									  ,(drop-last (internf "from-%s" sub-name))
									  ,(drop-last (internf "in-%s" sub-name))))
		(setq *units-map*
			  (alist>> *units-map* 
					   (read (concat ":" (format "%s" (drop-last name))))
					   (alist>> :from (eval (read (format "#'%s" (drop-last (internf "from-%s" name)))))
								:in (eval (read (format "#'%s" (drop-last (internf "in-%s" name))))))
					   (read (concat ":" (format "%s" (drop-last sub-name))))
					   (alist>> :from (eval (read (format "#'%s" (drop-last (internf "from-%s" sub-name)))))
								:in (eval (read (format "#'%s" (drop-last (internf "in-%s" sub-name))))))))
		(print name)
		(print num)))
(provide 'units)



