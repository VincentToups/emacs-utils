;; This is a port of David Van Horn's SRFI 101, the copyright
;; upon which is reproduced here given that this port is 
;; significantly similar as to constitute a "substantial portion."

;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. REMEMBER, THERE IS NO SCHEME UNDERGROUND. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(require 'cl)
(require 'recur)


(defstruct pl-cons size tree rest)
(defstruct pl-node val left right)

(defun pl-cons (size tree rest)
  (make-pl-cons :size size
				:tree tree
				:rest rest))

(defalias 'pl-node? #'pl-node-p)
(defalias 'pl-cons? #'pl-cons-p)

(defun pl-node (val left right)
  (make-pl-node
   :val val
   :left left
   :right right))

(defun sub1 (n) (- n 1))
(defun add1 (n) (+ n 1))

(defun tree-val (tr)
  (if (pl-node? tr)
	  (pl-node-val tr)
	tr))

(defun tree-map (f tr)
  (if (pl-node? tr)
	  (pl-node (funcall f (pl-node-val tr))
			   (tree-map f (pl-node-left tr))
			   (tree-map f (pl-node-right tr)))
	(funcall f tr)))

(defun tree-for-each (f tr)
  (if (pl-node? tr)
	  (pl-node (funcall f (pl-node-val tr))
			   (tree-for-each f (pl-node-left tr))
			   (tree-for-each f (pl-node-right tr)))
	(funcall f tr)))

(defmacro named-let (name binders &rest body)
  (let ((vars (mapcar #'car binders))
		(initials (mapcar #'cadr binders)))
	`(labels ((,name ,vars ,@body))
	   (,name ,@initials))))

(defun tree-map/n (f ts)
  (named-let 
   recr ((ts ts))
   (if (and (pair? ts)
			(pl-node? (car ts)))
	   (pl-node (apply f (mapcar #'pl-node-val ts))
				(recr (mapcar #'pl-node-left ts))
				(recr (mapcar #'pl-node-right ts)))
	 (apply f ts))))

(defun tree-for-each/n (f ts)
  (named-let 
   recr ((ts ts))
   (if (and (pair? ts)
			(pl-node? (car ts)))
	   (progn (apply f (mapcar #'pl-node-val ts))
			  (recr (mapcar #'pl-node-left ts))
			  (recr (mapcar #'pl-node-right ts)))
	 (apply f ts))))

(defun half (i)
  (ash i -1))

(defun build-tree (i f)
  (named-let 
   rec
   ((i i)
	(o 0))
   (if (= i 1)
	   (funcall f o)
	 (let ((i/2 (half i)))
	   (pl-node (funcall f o)
				(rec i/2 (add1 0))
				(rec i/2 (+ 1 o i/2)))))))

(defun tr:make-tree (i x)
  (named-let recr ((i i))
			 (if (= 1 i) 
				 x
			   (let ((n (recr (half i))))
				 (pl-node x n n)))))

(defun tree-ref/update (mid tr i f)
  (cond ((= i 0)
		 (if (pl-node? tr)
			 (values (pl-node-val tr)
					 (pl-node
					  (funcall f (pl-node-val tr))
					  (pl-node-left tr)
					  (pl-node-right tr)))
		   (values tr (funcall f tr))))
		((<= i mid)
		 (multiple-value-bind (v* t*) (tree-ref/update (half (sub1 mid))
													   (pl-node-left tr)
													   (sub1 i)
													   f)
		   (values v* (pl-node (pl-node-val tr)
							   t*
							   (pl-node-right tr)))))
		(t
		 (multiple-value-bind (v* t*) (tree-ref/update (half (sub1 mid))
													   (pl-node-right tr)
													   (sub1 (- i mid))
													   f)
		   (values v* (pl-node (pl-node-val tr)
							   (pl-node-left tr)
							   t*))))))

(defun tree-ref/a (tr i mid) 
  (cond ((zero? i) (tree-val tr))
		((<= i mid) 
		 (tree-ref/a (pl-node-left tr) 
					 (sub1 i) 
					 (half (sub1 mid))))
		(else 
		 (tree-ref/a (pl-node-right tr) 
					 (sub1 (- i mid)) 
					 (half (sub1 mid))))))

(defun tree-ref (size tr i)
  (if (zero? i)
	  (tree-val tr)
	(tree-ref/a tr i (half (sub1 size)))))

(defun tree-update (size tr i f)
  (named-let recr ((mid (half (sub1 size))) (tr tr) (i i))
			 (cond ((zero? i)
					(if (pl-node? tr)
						(pl-node (f (pl-node-val tr))
								 (pl-node-left tr)
								 (pl-node-right tr))
					  (funcall f tr)))
				   ((<= i mid)
					(pl-node (pl-node-val tr) 
							 (recr (half (sub1 mid))
								   (pl-node-left tr) 
								   (sub1 i)) 
							 (pl-node-right tr)))
				   (else
					(pl-node (pl-node-val tr) 
							 (pl-node-left tr) 
							 (recr (half (sub1 mid))
								   (pl-node-right tr) 
								   (sub1 (- i mid))))))))



(defvar ra:null (quote ()))

;; [Any -> Boolean]
(defalias 'ra:pair? #'pl-cons?)


;; [Any -> Boolean]
(defalias 'ra:null? #'null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(defun ra:cons (x ls)
  (if (pl-cons? ls)
	  (let ((s (pl-cons-size ls)))
		(if (and (pl-cons? (pl-cons-rest ls))
				 (= (pl-cons-size (pl-cons-rest ls))
					s))
			(pl-cons (+ 1 s s) 
					 (pl-node x 
							  (pl-cons-tree ls)
							  (pl-cons-tree (pl-cons-rest ls)))
					 (pl-cons-rest (pl-cons-rest ls)))
		  (pl-cons 1 x ls)))
	(pl-cons 1 x ls)))

(defun ra:car+cdr 
  (p)
  (assert (pl-cons? p))
  (if (pl-node? (pl-cons-tree p))
	  (let ((s* (half (pl-cons-size p))))
		(values (tree-val (pl-cons-tree p))
				(pl-cons s* 
						 (pl-node-left (pl-cons-tree p))
						 (pl-cons s*
								  (pl-node-right (pl-cons-tree p))
								  (pl-cons-rest p)))))
	(values (pl-cons-tree p) (pl-cons-rest p))))

(defun ra:car (p)
  (car (ra:car+cdr p)))

(defun ra:cdr (p)
  (cadr (ra:car+cdr p)))

(defun ra:list-ref/update (ls i f)
  (named-let recr ((xs ls) (j i))
			 (if (< j (pl-cons-size xs))
				 (multiple-value-bind (v* t*) 
					 (tree-ref/update (half (sub1 (pl-cons-size xs))) 
									  (pl-cons-tree xs) j f)
				   (values v* (pl-cons (pl-cons-size xs) 
									   t* 
									   (pl-cons-rest xs))))
			   (multiple-value-bind (v* r*) 
				   (recr (pl-cons-rest xs) 
						 (- j (pl-cons-size xs)))
				 (values v* (pl-cons (pl-cons-size xs) 
									 (pl-cons-tree xs) 
									 r*))))))
(defun ra:list-update (ls i f)
  (named-let recr ((xs ls) (j i))
			 (let ((s (pl-cons-size xs)))
			   (if (< j s) 
				   (pl-cons s (tree-update s (pl-cons-tree xs) j f) (pl-cons-rest xs))
				 (pl-cons s (pl-cons-tree xs) (recr (pl-cons-rest xs) (- j s)))))))

(defun ra:list-ref/set (ls i v)
  (ra:list-ref/update ls i 
					  (enclose (v)
							   (lambda (_) v))))

(defun fold-right (f init xs)
  (reduce f xs :initial-value init :from-end 'right))

(defun ra:list (&rest xs)
  (fold-right #'ra:cons ra:null xs))

(defun* ra:make-list (k &optional (obj 0))
  (recur-let ((n k) (a ra:null))
			 (cond ((zero? n) a)
				   (else 
					(let ((tr (largest-skew-binary n)))
					  (recur (- n tr)
							 (pl-cons tr (tr:make-tree tr obj) a)))))))

(defun skew-succ (tr) (add1 (ash tr 1)))

(defun largest-skew-binary (n)
  (if (= 1 n) 
	  1
	(let* ((tr (largest-skew-binary (half n)))
		   (s (skew-succ tr)))
	  (if (> s n) tr s))))

(recur-defun* ra:list? (x)
  (cond 
   ((ra:null? x) t)
   ((not (pl-cons? x)) nil)
   (t
	(recur (pl-cons-rest x)))))

(defalias 'ra:caar (lambda (ls) (ra:car (ra:car ls))))
(defalias 'ra:cadr (lambda (ls) (ra:car (ra:cdr ls))))
(defalias 'ra:cddr (lambda (ls) (ra:cdr (ra:cdr ls))))
(defalias 'ra:cdar (lambda (ls) (ra:cdr (ra:car ls))))

(defalias 'ra:caaar (lambda (ls) (ra:car (ra:car (ra:car ls)))))
(defalias 'ra:caadr (lambda (ls) (ra:car (ra:car (ra:cdr ls)))))
(defalias 'ra:caddr (lambda (ls) (ra:car (ra:cdr (ra:cdr ls)))))
(defalias 'ra:cadar (lambda (ls) (ra:car (ra:cdr (ra:car ls)))))
(defalias 'ra:cdaar (lambda (ls) (ra:cdr (ra:car (ra:car ls)))))
(defalias 'ra:cdadr (lambda (ls) (ra:cdr (ra:car (ra:cdr ls)))))
(defalias 'ra:cdddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr ls)))))
(defalias 'ra:cddar (lambda (ls) (ra:cdr (ra:cdr (ra:car ls)))))

(defalias 'ra:caaaar (lambda (ls) (ra:car (ra:car (ra:car (ra:car ls))))))
(defalias 'ra:caaadr (lambda (ls) (ra:car (ra:car (ra:car (ra:cdr ls))))))
(defalias 'ra:caaddr (lambda (ls) (ra:car (ra:car (ra:cdr (ra:cdr ls))))))
(defalias 'ra:caadar (lambda (ls) (ra:car (ra:car (ra:cdr (ra:car ls))))))
(defalias 'ra:cadaar (lambda (ls) (ra:car (ra:cdr (ra:car (ra:car ls))))))
(defalias 'ra:cadadr (lambda (ls) (ra:car (ra:cdr (ra:car (ra:cdr ls))))))
(defalias 'ra:cadddr (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:cdr ls))))))
(defalias 'ra:caddar (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:car ls))))))
(defalias 'ra:cdaaar (lambda (ls) (ra:cdr (ra:car (ra:car (ra:car ls))))))
(defalias 'ra:cdaadr (lambda (ls) (ra:cdr (ra:car (ra:car (ra:cdr ls))))))
(defalias 'ra:cdaddr (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:cdr ls))))))
(defalias 'ra:cdadar (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:car ls))))))
(defalias 'ra:cddaar (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:car ls))))))
(defalias 'ra:cddadr (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:cdr ls))))))
(defalias 'ra:cddddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:cdr ls))))))
(defalias 'ra:cdddar (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:car ls))))))

(defun ra:length (ls)
  (assert (ra:list? ls))
  (named-let recr ((ls ls))
			 (if (pl-cons? ls)
				 (+ (pl-cons-size ls) (recr (pl-cons-rest ls)))
			   0)))

(defun make-foldl (empty? first rest)
  (enclose (empty? first rest)
		   (labels ((f (cons empty ls)
					   (if (funcall empty? ls) 
						   empty
						 (f cons
							(funcall cons (funcall first ls) empty) 
							(funcall rest ls)))))
			 #'f)))

(defun make-foldr (empty? first rest)
  (enclose (empty? first rest)
		   (labels ((f (cons empty ls)
					   (if (funcall empty? ls) 
						   empty
						 (funcall cons (funcall first ls)
								  (f cons empty (funcall rest ls))))))
			 #'f)))


(defalias 'ra:foldl/1 (make-foldl #'ra:null? #'ra:car #'ra:cdr))
(defalias 'ra:foldr/1 (make-foldr #'ra:null? #'ra:car #'ra:cdr))

(defun ra:append (&rest lss)
  (cond ((null? lss) ra:null)
		(else (named-let recr ((lss lss))
						 (cond ((null? (cdr lss)) (car lss))
							   (else (ra:foldr/1 #'ra:cons
												 (recr (cdr lss))
												 (car lss))))))))

(defun ra:reverse (ls)
  (ra:foldl/1 #'ra:cons ra:null ls))

(defun ra:list-tail (ls i)
  (named-let recr ((xs ls) (j i))
			 (cond ((zero? j) xs)
				   (else (recr (ra:cdr xs) (sub1 j))))))

(defun ra:list-ref (ls i)
  (named-let recr ((xs ls) (j i))
			 (if (< j (pl-cons-size xs))
				 (tree-ref (pl-cons-size xs) (pl-cons-tree xs) j)
			   (recr (pl-cons-rest xs) (- j (pl-cons-size xs))))))

(defun ra:list-set (ls i v)
  (multiple-value-bind (_ l*) (ra:list-ref/set ls i v) l*))

(recur-defun* ra:none-null (lists)
  (if (empty? lists) t
	(let ((first (car lists))
		  (rest (cdr lists)))
	  (if (ra:null? first) nil
		(recur rest)))))

(defun* ra:multimap (f &rest the-lists) 
  (cond ((ra:null? (car the-lists)) ra:null)
		(else
		 ;; IMPROVE ME: make one pass over the-lists.
		 (pl-cons (pl-cons-size (car the-lists))
				  (tree-map/n f (mapcar #'pl-cons-tree the-lists))
				  (apply #'ra:multimap f (mapcar #'pl-cons-rest the-lists))))))

(defun ra:map
  (f &rest lss)
  (cond
   ((length=1 lss)
	(named-let recr ((ls (car lss)))
			   (if (pl-cons? ls)
				   (pl-cons (pl-cons-size ls) 
							(tree-map f (pl-cons-tree ls)) 
							(recr (pl-cons-rest ls)))
				 ra:null)))
   (t (ra:multimap f lss))))

(defun ra:for-each (f &rest lss)
  (cond
   ((length=1 lss)
	(let ((ls (car lss)))
	  (when (pl-cons? ls)
		(tree-for-each f (pl-cons-tree ls))
		(ra:for-each f (pl-cons-rest ls)))))
   (t
   	(named-let recr ((lss lss))
   			   (when (ra:pair? (car lss))
   				 (tree-map/n f (mapcar #'pl-cons-tree lss))
   				 (recr (mapcar #'pl-cons-rest lss)))))))

(defun ra:random-access-list->linear-access-list (x)
  (ra:foldr/1 #'cons '() x))

(defun ra:linear-access-list->random-access-list (x)
  (fold-right #'ra:cons '() x))

(defalias 'get-cached
  (lexical-let ((h (make-hash-table :test 'eq)))
	(lambda (x)
	  (labels ((f (x)
				  (cond
				   ((pair? x) (ra:cons (f (car x)) (f (cdr x))))
				   ((vectorp x) (map 'vector #'f x))
				   (else x))))
		(cond
		 ((not (or (pair? x) (vectorp x))) x)
		 ((gethash h x nil))
		 (t
		  (let ((v (f x)))
			(puthash h x v)
			v)))))))

(defmacro ra:quote (datum)
  `(get-cached ',datum))

(provide 'ra-lists)









