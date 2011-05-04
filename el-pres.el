(require 'utils)
(require 'scripting)
(require 'monad-parse)
(require 'functional)
(require 'cl)
(provide 'el-pres)

(eval-when-compile 
  (require 'cl))

(defun create-el-pres (dir)
  (interactive "G")
  (if (directoryp dir)
	  (error "You probably don't want to create a presenation in an extent directory (%s)" dir)
	(make-directory dir)
	(with-working-directory 
	 dir
	 (with-current-buffer (find-file "page-1.el")
	   (insert ";;; <Title>\n\n")
	   (insert "(dont-do (put-code-here))\n")
	   (insert ";;;Controls ")
	   (insert-button "Home"
					  'action
					  (lambda (button)
						(find-file "page-1.el")))))))

(defun =page ()
  (=string "page-"))

(defun =page-number ()
  (lexical-mlet 
   monad-parse 
   ((num (one-or-more (=satisfies #'digit-char?))))
   (m-return (string-to-number (coerce num 'string)))))


(defun parse-page-name (name)
  (parse-string 
   (lexical-mlet 
	monad-parse
	((_ (=page))
	 (n (=page-number))
	 (_ (=string ".el")))
	(m-return n)) 
   name))

(parse-page-name "page-1.el")

(defun subsequent-page-exists? (page-number)
  (file-exists-p (format "page-%d.el" (+ page-number 1))))
(defun previous-page-exists? (page-number)
  (file-exists-p (format "page-%d.el" (- page-number 1))))

(defun jump-to (button file)
  (with-current-buffer (find-file file)
	(rebuild-control-panel (current-buffer))
	(current-buffer)))

(defun jump-to-page-number (button n)
  (with-current-buffer (find-file (format "page-%d.el" n))
	(rebuild-control-panel)
	(current-buffer)))

(defun make-el-pres-page-unsafe (n)
  (with-current-buffer 
	  (find-file (format "page-%d.el" n))
	(insert ";;; <Title>\n\n")
	(insert "(dont-do (put-code-here))\n")
	(insert ";;;Controls ")
	(if (previous-page-exists? n)
		(insert-button "<<<"
					   'action (par #'jump-to 
									(format "page-%d.el" (- n 1)))))
	(insert " . ")
	(if (subsequent-page-exists? n)
		(insert-button "<<<"
					   'action (par #'jump-to 
									(format "page-%d.el" (+ n 1)))))
	(insert "   ")
	(save-buffer)))

(defun any-pages? (ls-results)
  (not (string= "ls:"
				(substring (car ls-results) 0 3))))

(defun sort-by-page-descending (page-list)
  (mapcar #'car (functional-sort 
				 (zip page-list (mapcar #'parse-page-name page-list))
				 (decorate-all #'> #'cadr))))

(defun sort-by-page-ascending (page-list)
  (mapcar #'car (functional-sort 
				 (zip page-list (mapcar #'parse-page-name page-list))
				 (decorate-all #'< #'cadr))))


(defun increment-page-name (page-name)
  (format "page-%d.el" (+ 1 (parse-page-name page-name))))

(defun* safe-get-pages (&optional (dir (wd)))
  (with-working-directory 
   dir
   (let ((result (sh "ls page-*.el")))
	 (if (any-pages? result)
		 result
	   nil))))

(defun get-pages-by-number-predicate (dir predicate)
  (with-working-directory 
   dir
   (let ((files (sort-by-page-ascending (safe-get-pages dir))))
	 (filter
	  (decorate-n predicate 0 #'parse-page-name) files))))

(defun* get-pages-at-or-above (n &optional (dir (wd)))
  (get-pages-by-number-predicate dir
								 (par #'>= n)))

(defun* get-pages-above (n &optional (dir (wd)))
  (get-pages-by-number-predicate dir
								 (par #'> n)))



(defun* move-pages-up (starting-with &optional (dir (wd)))
  (with-working-directory 
   dir
   (let ((reopen nil))
	 (mlet monad-maybe^i 
		   ((pages (reverse (get-pages-at-or-above starting-with dir)))
			(new-pages (mapcar #'increment-page-name pages)))
		   (loop for p in pages and n in new-pages do
				 (if (file-exists-p n)
					 (error "renamed page %s already exists, which should be impossible." n))
				 (let-if buffer (get-buffer p) 
						 (with-current-buffer buffer
						   (push (buffer-name buffer) reopen)
						   (save-buffer)
						   (kill-buffer)))
				 (rename-file p n))
		   (loop for r in reopen do
				 (find-file-noselect r))))))

(defun* el-pres-add-page-after (&optional (dir (wd)))
  (interactive)
  (with-working-directory dir
						  (let* ((current-page (parse-page-name (buffer-name (current-buffer)))))
							(move-pages-up (+ 1 current-page) dir)
							(make-el-pres-page-unsafe (+ 1 current-page))
							(rebuild-control-panel))))

(defun get-end-of-line ()
  (save-excursion 
	(end-of-line) 
	(point)))

(defun* insert-page-number-buttons (&optional (buffer (current-buffer)) (dir (wd)))
  (let ((pages (sort-by-page-ascending (safe-get-pages dir)))
		(current-page (parse-page-name (buffer-name buffer))))
	(loop for page in pages do 
		  (let ((pn (parse-page-name page)))
			(if (= pn current-page)
				(insertf "%d" pn)
			  (insert-button 
			   (format "%d" pn) 
			   'action
			   (par #'jump-to-page-number pn)))
			(insert "   ")))))

(defun* rebuild-control-panel (&optional (buffer (current-buffer)))
  (interactive)
  (with-current-buffer buffer
	(save-buffer)
	(let ((page-number (parse-page-name (buffer-name buffer))))
	  (save-excursion 
		(goto-char (point-max))
		(if (search-backward ";;;Controls" nil t)
			(goto-char (match-beginning 0))
		  (progn (goto-char (point-max))
				 (insert "\n")))
		(beginning-of-line)
		(delete-region (point)
					   (point-max))
		(insert ";;;Controls ")
		(insert-button "Home"
					   'action
					   (par #'jump-to "page-1.el"))
		(insert "   ")
		(if (> page-number 1)
			(insert-button "<<<"
						   'action
						   (par #'jump-to-page-number (- page-number 1))))
		(insert " . ")
		(if (file-exists-p (format "page-%d.el" (+ page-number 1)))
			(insert-button ">>>"
						   'action (par #'jump-to-page-number (+ page-number 1))))
		(insert "   ")
		(insert-page-number-buttons buffer)
		(insert "\n")
		(insert ";;;")
		(insert "         ")
		(insert-button "Index"
					   'action
					   (lambda (button)
						 (el-pres-build-index)))
		)
	  (save-buffer))))


(defun* rebuild-control-panels (&optional (dir (wd)))
  (interactive)
  (with-working-directory dir 
						  (let ((pages (safe-get-pages dir)))
							(loop for page in pages do
								  (let* ((page-number (parse-page-name page))
										 (already-open? (get-buffer page))
										 (buffer (if already-open? already-open? 
												   (find-file-noselect page))))
									(if already-open? 
										(rebuild-control-panel buffer)))))))

(defun get-buffer-or-find-file (name)
  (let ((get? (get-buffer name))
		(buf  (find-file-noselect name)))
	(list buf get?)))

(defun* get-page-title (&optional (page (buffer-name (current-buffer))))
  (let-seq (buf open-already) (get-buffer-or-find-file page)
		   (with-current-buffer buf
			 (save-excursion 
			   (goto-char (point-min))
			   (chomp (prog1 (replace-regexp-in-string 
							  "\\(;;;\\).*\\'" "" 
							  (buffer-subline)
							  nil
							  nil 
							  1)
						(if (not open-already)
							(kill-buffer))))))))

(defun* el-pres-build-index (&optional (dir (wd)))
  (let ((pages (sort-by-page-ascending (safe-get-pages dir))))
	(with-working-directory 
	 dir
	 (with-current-buffer (find-file "index.el")
	   (delete-region (point-min) (point-max))
	   (loop for page in pages do
			 (insert ";;; - ")
			 (insert-button 
			  (format "%s" (get-page-title page))
			  'action
			  (par #'jump-to page))
			 (insert "\n"))))))
