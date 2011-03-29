(require 'utils)

(setf *animator-color-stack* nil)

(defun start-animator-process ()
  (let ((sh (shell "*animator*")))
	(comint-send-strings sh "animator")
	(setq *animator* sh)))
(start-animator-process)

(defun animator-send (&rest strings)
  (apply #'comint-send-strings *animator* strings))

(defun animator-flush ()
  (comint-send-strings *animator* "flush"))
(defun animator-frame ()
  (comint-send-strings *animator* "frame"))

(defun animator-color (cc)
  (if (listp cc)
	  (let* ((s (format "%s" cc))
			 (n (length s)))
		(animator-send (concat "color " (substring s 1 (- n 1)))))
	(animator-send (format "color %s" cc))))

(defun animator-push-color (cc)
  (push cc *animator-color-stack*)
  (if (listp cc)
	  (let* ((s (format "%s" cc))
			 (n (length s)))
		(animator-send (concat "color " (substring s 1 (- n 1)))))
	(animator-send (format "color %s" cc))))
(defun animator-pop-color ()
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

(defun animator-dot (x y)
  (comint-send-strings *animator*
					   (format "dot %f %f" x y)))

(defun animator-dots (pairs)
  (loop for pair in pairs do
		(apply #'animator-dot pair)))

(defun animator-line (x1 y1 x2 y2)
  (animator-send (format "line %f %f %f %f" x1 y1 x2 y2)))

(defun animator-disjoint-lines (&rest lines)
  (loop for line in lines do 
		(apply animator-line line)))

(defun animator-connected-lines (&rest args)
  (let ((args (flatten args)))
	(animator-send 
	 (concat "lines "
			 (foldl (lambda (it ac) (concat ac (format " %f" it)))
					""
					args)))))

(defun animator-poly (&rest args)
  (let ((args (flatten args)))
	(animator-send 
	 (concat "poly "
			 (foldl (lambda (it ac) (concat ac (format " %f" it)))
					""
					args)))))

(defun animator-circle (x y r &optional verts)
  (let ((verts (if verts verts 25)))
	(animator-send (format "circle %f %f %f %f" x y r verts))))

(defun animator-filled-circle (x y r &optional verts)
  (let ((verts (if verts verts 25)))
	(animator-send (format "fillcircle %f %f %f %f" x y r verts))))

(defun animator-text (x y text &optional alignment)
  (if alignment
	  (animator-send (format "text %s %f %f \"%s\"" alignment x y text))
	(animator-send (format "text %f %f \"%s\"" x y text))))

(defun animator-dup ()
  (animator-send "push"))

(defun animator-pop ()
  (animator-send "pop"))

(defun animator-ident ()
  (animator-send "ident"))

(defun animator-shift (x y)
  (animator-send (format "shift %f %f" x y)))

(defun animator-rotate (degrees)
  (animator-send (format "rotate %f" degrees)))

(defun animator-scale (x &optional y)
  (let ((y (if y y x)))
	(animator-send (format "scale %f %f" x y))))

(defun animator-viewport (left bottom w h)
  (animator-send (format "viewport %f %f %f %f" left bottom w h)))

(defun create-animator-format-string (name-string args)
  (loop for i from 1 to (length args) do
		(setq name-string (concat name-string " %f")))
  name-string)

(defmacro* def-numeric-animator-primitive (name &optional doc &rest arg-names)
  (let ((interface-name (internf "animator-%s" name))
		(name-string (format "%s" name))
		(doc (if (stringp doc) doc nil))
		(arg-names (if (stringp doc) arg-names
					 (cons doc arg-names))))
	`(defun ,interface-name ,arg-names ,doc
	   (animator-send (format ,(create-animator-format-string name-string arg-names)
					  ,@arg-names)))))

(defmacro* def-numeric-animator-primitive-alt-name (interface-name name &optional doc &rest arg-names)
  (let ((name-string (format "%s" name))
		(doc (if (stringp doc) doc nil))
		(arg-names (if (stringp doc) arg-names
					 (cons doc arg-names))))
	`(defun ,interface-name ,arg-names ,doc
	   (animator-send (format ,(create-animator-format-string name-string arg-names)
					  ,@arg-names)))))


(def-numeric-animator-primitive thick "Set animator line thickness"
  thickness)

(def-numeric-animator-primitive alpha "Set animator transparency." alpha)

(def-numeric-animator-primitive arrow "Draw an arrow pointing towards X2 Y2"
  x1 y1 x2 x2)

(def-numeric-animator-primitive-alt-name fill-rect fillrect "Fill an animator rectangle with the current color." x y w h)

(def-numeric-animator-primitive rect "Fill an animator rectangle with the current color." x y w h)

(with-flush/frame (animator-rect 0 0 .25 .25))




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