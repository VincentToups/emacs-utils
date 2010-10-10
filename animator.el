(require 'utils)

(setf *animator-funs* (alist>>))
(setf *animator-color-stack* nil)


(defmacro* with-animator (&body body)
  `(labels ,(loop for info in *animator-funs* collect
				  (let ((actual-name (car info))
						(local-name (car (cadr  info)))
						(arglist (cadr (cadr info)))
						(argsym (gensym "animator-args-")))
					`(,local-name (&rest ,argsym) (apply #',actual-name ,argsym))))
	 ,@body))
(defmacro* defanimatorfun (name arglist docstring &body body)
  (let ((actual-name (internf "animator-%s" name)))
	(setf *animator-funs* (alist>> *animator-funs*
								   actual-name (list name arglist)))
	`(defun ,actual-name ,arglist ,docstring (with-animator ,@body))))
(defun start-animator-process ()
  (let ((sh (shell "*animator*")))
	(comint-send-strings sh "animator")
	(setq *animator* sh)))
(start-animator-process)

(defanimatorfun send (&rest strings)
  (apply #'comint-send-strings *animator* strings))

(defanimatorfun flush ()
  (comint-send-strings *animator* "flush"))
(defanimatorfun frame ()
  (comint-send-strings *animator* "frame"))

(defanimatorfun color (cc)
  (if (listp cc)
	  (let* ((s (format "%s" cc))
			 (n (length s)))
		(animator-send (concat "color " (substring s 1 (- n 1)))))
	(animator-send (format "color %s" cc))))

(defanimatorfun push-color (cc)
  (push cc *animator-color-stack*)
  (if (listp cc)
	  (let* ((s (format "%s" cc))
			 (n (length s)))
		(animator-send (concat "color " (substring s 1 (- n 1)))))
	(animator-send (format "color %s" cc))))
(defanimatorfun pop-color ()
  (let ((clr (pop *animator-color-stack*))
		(top (car *animator-color-stack*)))
	(if top (animator-color top)
	  (animator-color "black"))))



(defmacro* with-animator-color (color &body body)
  (let ((color-sym (gensym "animator-color-")))
	`(let ((,color-sym ,color))
	   (animator-push-color ,color-sym)
	   ,@body
	   (animator-pop-color))))


(defmacro* with-flush/frame (&body body)
  `(progn (animator-frame) ,@body (animator-flush)))

(defanimatorfun dot (x y)
  (comint-send-strings *animator*
					   (format "dot %f %f" x y)))

(defanimatorfun dots (pairs)
  (loop for pair in pairs do
		(apply #'animator-dot pair)))

(defanimatorfun line (x1 y1 x2 y2)
  (animator-send (format "line %f %f %f %f" x1 y1 x2 y2)))

(defanimatorfun disjoint-lines (&rest lines)
  (loop for line in lines do 
		(apply animator-line line)))

(defanimatorfun connected-lines (&rest args)
  (let ((args (flatten args)))
	(animator-send 
	 (concat "lines "
			 (foldl (lambda (it ac) (concat ac (format " %f" it)))
					""
					args)))))

(defanimatorfun poly (&rest args)
  (let ((args (flatten args)))
	(animator-send 
	 (concat "poly "
			 (foldl (lambda (it ac) (concat ac (format " %f" it)))
					""
					args)))))

(defanimatorfun circle (x y r &optional verts)
  (let ((verts (if verts verts 25)))
	(animator-send (format "circle %f %f %f %f" x y r verts))))

(defanimatorfun filled-circle (x y r &optional verts)
  (let ((verts (if verts verts 25)))
	(animator-send (format "fillcircle %f %f %f %f" x y r verts))))

(defanimatorfun text (x y text &optional alignment)
  (if alignment
	  (animator-send (format "text %s %f %f \"%s\"" alignment x y text))
	(animator-send (format "text %f %f \"%s\"" x y text))))




(dont-do (with-flush/frame (animator-text 0 0 "HOLY SHEET!"))
		 (loop for i from 1 to 1000000 do
			   (sleep-for 0 10)
			   (with-flush/frame 
				(print (* 50.0 (sin (/ i 100.0))))
				(animator-text 0
							   (* 50.0 (sin (/ i 100.0)))
							   "HOLY SHEEET!!!")))

		 (with-animator 
		  (with-flush/frame (text 0 0 "SUP DAWG")))

		 (with-animator
		  (with-flush/frame 
		   (animator-poly '(-0.5 -0.5) '(-0.5 0.5) '(0.5 0.5) '(0.5 -0.5))
		   (with-animator-color "blue"
								(animator-poly '(-0.25 -0.25) '(-0.25 0.25) '(0.25 0.25) '(0.25 -0.25)))
		   (animator-poly '(-0.125 -0.125) '(-0.125 0.125) '(0.125 0.125) '(0.125 -0.125))))
		 (with-animator 
		  (color "green"))
		 )


(provide 'animator)