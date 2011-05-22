 (require 'with-stack)
(require 'stack-words)
(require 'eperiodic)
(require 'defn)
(require 'monads)
(require 'functional)
(require 'units)
(require 'recur)

(defun element-name (element)
  (||| 'name {element} 2>assoc 1>cdr))
(defun element-symbol (element)
  (||| 'symbol {element} 2>assoc 1>cdr))
(defun element-name (element) (||| 'name {element} 2>assoc 1>cdr))
(defun element-symbol (element) (||| 'symbol {element} 2>assoc 1>cdr))
(defun element-atomic-mass (element)
  (||| 'atomic-mass {element} 2>assoc 1>cdr))
(defun element-density (element) (||| 'density {element} 2>assoc 1>cdr))
(defun element-melting-point (element)
  (||| 'melting-point {element} 2>assoc 1>cdr))
(defun element-boiling-point (element)
  (||| 'boiling-point {element} 2>assoc 1>cdr))
(defun element-atomic-radius (element)
  (||| 'atomic-radius {element} 2>assoc 1>cdr))
(defun element-covalent-radius (element)
  (||| 'covalent-radius {element} 2>assoc 1>cdr))
(defun element-ionic-radius (element)
  (||| 'ionic-radius {element} 2>assoc 1>cdr))
(defun element-atomic-volume (element)
  (||| 'atomic-volume {element} 2>assoc 1>cdr))
(defun element-specific-heat (element)
  (||| 'specific-heat {element} 2>assoc 1>cdr))
(defun element-fusion-heat (element)
  (||| 'fusion-heat {element} 2>assoc 1>cdr))
(defun element-evaporation-heat (element)
  (||| 'evaporation-heat {element} 2>assoc 1>cdr))
(defun element-thermal-conductivity (element)
  (||| 'thermal-conductivity {element} 2>assoc 1>cdr))
(defun element-debye-temperature (element)
  (||| 'debye-temperature {element} 2>assoc 1>cdr))
(defun element-pauling-negativity-number (element)
  (||| 'pauling-negativity-number {element} 2>assoc 1>cdr))
(defun element-first-ionization-energy (element)
  (||| 'first-ionization-energy {element} 2>assoc 1>cdr))
(defun element-oxidation-states (element)
  (||| 'oxidation-states {element} 2>assoc 1>cdr))
(defun element-lattice-structure (element)
  (||| 'lattice-structure {element} 2>assoc 1>cdr))
(defun element-lattice-constant (element)
  (||| 'lattice-constant {element} 2>assoc 1>cdr))
(defun element-lattice-c/a-ratio (element)
  (||| 'lattice-c/a-ratio {element} 2>assoc 1>cdr))
(defun element-appearance (element)
  (||| 'appearance {element} 2>assoc 1>cdr))
(defun element-discovery-date (element)
  (||| 'discovery-date {element} 2>assoc 1>cdr))
(defun element-discovered-by (element)
  (||| 'discovered-by {element} 2>assoc 1>cdr))
(defun element-named-after (element)
  (||| 'named-after {element} 2>assoc 1>cdr))

(defn get-element-by-name
  ([name [element & rest :as elements]]
   (cond
	((or (string= (upcase name) (upcase (element-name element)))
		 (string= (upcase name) (upcase (element-symbol element))))
	 element)
	((not elements) nil)
	(t (recur name rest))))
  ([name]
   (get-element-by-name name eperiodic-element-properties)))

(get-element-by-name "Helium" eperiodic-element-properties)
(get-element-by-name "He")

(defun less-electronegative-than (a b)
  (let ((en1 (read (element-pauling-negativity-number a)))
		(en2 (read (element-pauling-negativity-number b))))
	(cond ((and (numberp en1)
				(numberp en2))
		   (< en1 en2))
		  ((and (symbolp en1)
				(numberp en2))
		   nil)
		  ((and (numberp en1)
				(symbolp en2))
		   t)
		  ((and (symbolp en1)
				(symbolp en2))
		   nil))))

(defun more-electronegative-than (a b)
  (let ((en1 (read (element-pauling-negativity-number a)))
		(en2 (read (element-pauling-negativity-number b))))
	(cond ((and (numberp en1)
				(numberp en2))
		   (> en1 en2))
		  ((and (symbolp en1)
				(numberp en2))
		   nil)
		  ((and (numberp en1)
				(symbolp en2))
		   t)
		  ((and (symbolp en1)
				(symbolp en2))
		   nil))))

(setf chemical-names (||| {eperiodic-element-properties} '(1>element-symbol) map))

(defun generate-conditions (alist)
  (foldl (lambda (it ac)
		   (domonad< monad-seq 
					[a-case ac
							component (cadr it)]
					(cons (list (car it) component) a-case)))
		 (domonad< monad-seq 
				  [q (cadr (car alist))]
				  (list (list (car (car alist)) q)))
		 (cdr alist)))
(defun generate-conditions>> (&rest rest)
  (generate-conditions (apply #'alist>> rest)))

(defun sort-condition (condition-list key)
  (functional-sort condition-list 
				   (lambda (a b) (< (alist a key) (alist b key)))))

(defun group-by-condition (condition-list key &optional randomize)
  (mapcar #'cadr (foldl 
				  (lambda (it ac)
					(let ((val (alist it key)))
					  (alist-cons ac val it)))
				  ()
				  condition-list)))
(defun ungroup (grouped-condition-list)
  (flatten-once grouped-condition-list))

(defun add-permutations (conditions-list condition-name values)
  (domonad< monad-seq [c conditions-list
						v values]
		   (alist>> c condition-name v)))

(defun keyword->string (kw)
  (let ((s (format "%s" kw)))
	(substring s 1 (length s))))

(defun condition->filename (condition)
  (||| lisp-val: (let ((keys (mapcar #'car condition)))
				   (foldl (lambda (it ac)
							(concatf (list ac "=%s=%0.6d")
									 (keyword->string (car it))
									 (cadr it)))
						  ""
						  condition))
	   dup 1>length 1 swap 3>substring))

(defun dsf-prep (str)
  (let-repeatedly str 
				  (replace-regexp-in-string (rxq ".txt") "" str)
				  (replace-regexp-in-string "^.*/" "" str)))

(defun string->kw (s) 
  (read (concat ":" s)))

(defun* dsf (str &optional (field nil) (sep (rxq "=")))
  "dsf decomposes the filename in STR into an alist."
  (let* ((parts (split-string (dsf-prep str) sep))
		 (fields (mapcar #'string->kw (even-indexed-elements parts)))
		 (vals   (mapcar #'read (odd-indexed-elements parts)))
		 (alist (zip fields vals)))
	(if field (alist alist field)
	  alist)))

(defun* dsf-raw (str &optional (field nil) (sep (rxq "=")))
  "dsf decomposes the filename in STR into an alist."
  (let* ((parts (split-string (dsf-prep str) sep))
		 (fields (mapcar #'string->kw (even-indexed-elements parts)))
		 (vals   (odd-indexed-elements parts))
		 (alist (zip fields vals)))
	(if field (alist alist field)
	  alist)))



(defun* undsf (alist &optional (order (alist-fields alist)) (delim "="))
  (join (foldl
		 (lambda (pair flat)
		   (append flat (list (kw->string (car pair)) (to-string (cadr pair)))))
		 nil
		 alist) delim))

(defun* undsf-camel (alist &optional (order (alist-fields alist)) (delim "="))
  (join (foldl
		 (lambda (pair flat)
		   (append flat (list (camel-case (kw->string (car pair))) (to-string (cadr pair)))))
		 nil
		 alist) delim))

(defun kw->string (kw)
  (let ((s (format "%s" kw)))
	(substring s 1 (length s))))

(defun print-condition (condition &optional handlers)
  (join (mapcar (lambda (condition)
				  (let* ((key (car condition))
						 (handler (alist handlers key))
						 (val (if handler (funcall handler (cadr condition))
								(cadr condition))))
					(format "%s: %s"
							(kw->string key)
							val))) condition) ",  "))


(defun generate-instructions (condition handler-alist final-volume volume-units)
  (format "for %s\n \t%s and fill to %f %sL"
		  (print-condition condition)
		  (join (mapcar 
				 (lambda (condition)
				   (let* ((key (car condition))
						  (handler (alist handler-alist key)))
					 (funcall handler (cadr condition)))) condition) "\n\t")
		  (funcall (alist-in *units-map* `(,volume-units :in)) final-volume)
		  (||| {volume-units} "%s" swap 2>format dup length 1 swap substring)))

(defun to-string (x) (format "%s" x))

(defun dilution-volume (target-volume stock-concentration desired-concentration)
  (/ (* target-volume desired-concentration) stock-concentration))



(defun concentration-handler (substance desired-concentration stock final-volume units)
  (let ((dv (dilution-volume final-volume stock desired-concentration)))
	(format "Mix %f %sL of %s stock" (funcall (alist-in *units-map* `(,units :in)) dv)
			(||| {units} "%s" swap 2>format dup length 1 swap substring) substance)))

(defcurryl hpo-handler #'concentration-handler
  "HPO")

(defcurryl da-handler  #'concentration-handler
  "Dopamine")

(defcurryl mcs-handler #'concentration-handler 
  "MCS")


(defvar concentrated-hpo-stock 9.791 "Concentration of concentrated HPO Stock.")

(in-milli (dilution-volume (from-milli 50) 9.791 (from-milli 1000)))

(comment 
 (require 'chemistry)
 (from-milli 9791.0))

(defvar *final-volume-for-mixing* (from-milli 25))

(defcurryr default-da-handler  #'da-handler  (from-milli 1)    *final-volume-for-mixing* :micro)
(defcurryr default-hpo-handler #'hpo-handler (from-milli 1000) *final-volume-for-mixing* :micro)
(defcurryr default-mcs-handler #'mcs-handler (from-milli 100)  *final-volume-for-mixing* :micro)

(defdecorated default-hpo-handler-micro #'default-hpo-handler 
  (lambda (arglist)
	(cons (from-micro (car arglist))
		  (cdr arglist))))

(defdecorated default-mcs-handler-micro #'default-mcs-handler 
  (lambda (arglist)
	(cons (from-micro (car arglist))
		  (cdr arglist))))

(defdecorated default-da-handler-nano #'default-da-handler 
  (lambda (arglist)
	(cons (from-nano (car arglist))
		  (cdr arglist))))



(defun default-ph (x)
  (format "pH of added buffer should be %f" (from-centi x)))

(setq default-handler-alist 
	  (alist>> 
	   :sampleMcs #'default-mcs-handler-micro
	   :samplePh #'default-ph
	   :sampleDa #'default-da-handler-nano
	   :sampleHpo #'default-hpo-handler-micro))

(defun alist-keys->org-mode-table-segment (alist keys)
  (join
   (mapcar 
	(decorate-n (pal #'format " %s ") 0 (pal #'alist alist))
	keys)
   "|"))

(defun* generate-experiment-files (condition-args n-trials &optional (mixing-volume (from-milli 50)))
  (print "WARNING Volumes other than 50 mil don't work correctly.")
  (let* ((*final-volume-for-mixing* mixing-volume)
		 (keys (mapcar #'car condition-args))
		 (raw-conditions (generate-conditions condition-args))
		 (conditions (||| {raw-conditions} 
						  :samplePh 2>group-by-condition
						  '( 1>permute-list ) map 1>ungroup))
		 (trials (range n-trials)))
	(let ((instructions (find-file "instructions.md"))
		  (log          (find-file "log.org")))
	  (with-current-buffer log
		(kill-region (point-min) (point-max))
		(insertf "| n | filename | trial | bufferPh | %s |\n" (alist-keys->org-mode-table-segment
													(mapcar (lambda (x) (list x x))
															keys) keys)))
	  (with-current-buffer instructions
		(kill-region (point-min) (point-max)))
	  (loop for c in conditions and i from 1 do
			(loop for trial in (add-permutations (list c) :trial trials) do
				  (with-current-buffer log
					(insertf "| | %s | %d  |7.40 | %s |\n"
							 (condition->filename trial)
							 (alist trial :trial)
							 (alist-keys->org-mode-table-segment trial keys))))
			(with-current-buffer instructions 
			  (insertf "%d.\t %s\n" i
					   (generate-instructions c default-handler-alist mixing-volume :milli)))))))

(defun* generate-ph-hpo-da-experiment-files (condition-args n-trials &optional (mixing-volume (from-milli 50)))
  (print "WARNING Volumes other than 50 mil don't work correctly.")
  (let* ((raw-conditions (generate-conditions condition-args))
		 (conditions (||| {raw-conditions} 
						 :samplePh 2>group-by-condition
						 '( 1>permute-list ) map 1>ungroup))
		(trials (range n-trials)))
	(let ((instructions (find-file "instructions.md"))
		  (log          (find-file "log.org")))
	  (with-current-buffer log
		(kill-region (point-min) (point-max))
		(insertf "| n | filename | trial | bufferPh | samplePh | sampleDa | sampleHpo |\n"))
	  (with-current-buffer instructions
		(kill-region (point-min) (point-max)))
	  (loop for c in conditions and i from 1 do
			(loop for trial in (add-permutations (list c) :trial trials) do
				  (with-current-buffer log
					(insertf "| | %s | %d  |7.40 | %f | %f | %f |\n"
							 (condition->filename trial)
							 (alist trial :trial)
							 (alist trial :samplePh)
							 (alist trial :sampleDa)
							 (alist trial :sampleHpo))))
			(with-current-buffer instructions 
			  (insertf "%d.\t %s\n" i
					   (generate-instructions c default-handler-alist mixing-volume :milli)))))))


(defun clipboard-kill-ring-save-string (str)
  (with-temp-buffer 
	(insert str)
	(clipboard-kill-ring-save (point-min)
							  (point-max))))

(defun forward-cell ()
  (search-forward-regexp (rxq "|")))

(defun next-name-cp ()
  (interactive)
  (forward-line 1)
  (let* ((line-str (buffer-subline-no-properties))
		 (parts (filter (f-not #'empty?) (split-string line-str "|")))
		 (filename (elt parts 1)))
	(clipboard-kill-ring-save-string (chomp filename))
	(beginning-of-line)
	(forward-cell)
	(insert "_")
	(org-cycle)))

(provide 'chemistry)