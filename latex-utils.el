(require 'utils)
(require 'defn)
(require 'monads)

(defn next-% []
  (let ((start-point (point)))
	(forward-char 1)
	(loop with found = nil and
		  done = nil
		  while (not (or found done))
		  do
		  (cond 
		   ((at-end?)
			(setf done t)
			(setf found nil))
		   ((= ?% (char-at-point))
			(setf done t)
			(setf found t))
		   (t 
			(forward-char 1)))
		  finally (return
				   (if found (point)
					 (progn (goto-char start-point) nil))))))

(defn number-figs []
  (let ((chapter 0)
		(fig-num 1))
	(loop while 
		  (search-forward-regexp (rx line-start "\\" (group (or "chapter" "begin{figure}"))) nil t)
		  do
		  (let ((m (match-string 1)))
			(print m)
			(print chapter)
			(print fig-num)
			(cond ((string= m "chapter")
				   (setf chapter (+ 1 chapter))
				   (setf fig-num 1))
				  ((string= m "begin{figure}")
				   (next-%)
				   (kill-line)
				   (insertf "%% Figure %d.%d" chapter fig-num)
				   (setf fig-num (+ 1 fig-num))))))))

(defn is-epsfile? [filename] 
  (dlet [len (length filename) ext-start (- len 4)]
	(and ($ len >= 4)
		 (string= (substring filename ext-start len) ".eps"))))

										;(is-epsfile? "test.eps")
										;(is-epsfile? "test.tex")

(defn get-eps-files []
  (mapcar (fn [n] (concat "./stand-in-figures/" n)) (directory-files "./stand-in-figures/" nil "\.eps$")))

(defn generate-figure-tex-lifter [files]
  (dlet [here (point)]
	(loop for f in files do
		  (insertf "
\\begin{figure}[htp]
  \\centering
  \\includegraphics[width=0.9\\textwidth]{%s}
  \\caption{%s}
  \\label{%s}
\\end{figure}
\\pagebreak[4]
\\clearpage
" f (replace-regexp-in-string "_" "-" f) f))
	))

(defn insert-fig [f]
  (interactive "Ffile:")
  (insertf "
\\begin{figure}[htp]
  \\centering
  \\includegraphics[width=0.9\\textwidth]{%s}
  \\caption{}
  \\label{%s}
\\end{figure}
" 
		   
		   (replace-regexp-in-string (regexp-quote "~/work/thesis_writing/") "./" f)
		   (concat "fig:"
				   (first (split-string (car (last (split-string f (regexp-quote "/")))) (regexp-quote "."))))))



(defn insert-stand-in-figures []
  (interactive)
  (generate-figure-tex-lifter (get-eps-files)))

(defn inkscape [filename]
  (interactive "F")
  (with-current-buffer (shell)
	(dlet [p (get-buffer-process (shell))]
	  (comint-send-string p (concat "inkscape " filename " &>/dev/null& 
")))))

(defn gimp [filename]
  (interactive "F")
  (with-current-buffer (shell)
	(dlet [p (get-buffer-process (shell))]
	  (comint-send-string p (concat "gimp " filename " &>/dev/null& 
")))))


(defn latex-em-region [p m]
  (interactive "r")
  (save-excursion 
	(goto-char m)
	(insert "}")
	(goto-char p)
	(backward-char)
	(insert " {\\em ")))

(defn latex-em-bf-region [p m]
  (interactive "r")
  (save-excursion 
	(goto-char m)
	(insert "}}")
	(goto-char p)
	(backward-char)
	(insert " {\\em {\\bf")))


(defn collect-all-labels []
  (save-excursion 
	(goto-char (point-min))
	(loop with done = nil and 
		  i = 0
		  while (not done) do
		  (setf done (not (search-forward-regexp "\\\\label{\\(.*\\)}" nil t)))
		  (setf i (+ i 1))
		  (if (> i 1000) (setf done t))
		  when (not done)
		  collect (match-string 1))))

(defn collect-refs-uniquely []
  (save-excursion 
	(goto-char (point-min))
	(loop with done = nil and
		  i = 0 and
          refs = nil
		  while (not done) do
		  (setf done (not (search-forward-regexp "\\\\ref{\\(.*\\)}" nil t)))
		  (if (> i 1000) (setf done t))
		  when (not done) do
		  (let ((m (match-string 1)))
			(if (not (member m refs))
				(push m refs)))
		  finally (return (reverse refs)))))

(defn extract-figures []
  (save-excursion 
	(goto-char (point-min))
	(loop with done = nil and
		  i = 0 and
		  figs = () and 
		  labs = ()
		  while (not done) do
		  (print i)
		  (setf i (+ 1 i))
		  (let ((more? (search-forward-regexp  (regexp-quote "\\begin{figure}") nil t))
				(start (progn (beginning-of-line) (point))))
			(if more? 
				(progn 
				  (search-forward-regexp "\\\\label{\\(.*\\)}" nil t)
				  (push (match-string 1) labs)
				  (search-forward-regexp (regexp-quote "\\end{figure}") nil t)
				  (let* ((end (progn (end-of-line) (point)))
						 (ss (buffer-substring start end)))
					(kill-region start end)
					(push ss figs)))
			  (setf done t)))
		  finally (return (list (reverse figs)  (reverse labs))))))



(defn collect-fig-refs-uniquely []
  (save-excursion 
	(goto-char (point-min))
	(loop with done = nil and
		  i = 0 and
		  refs = nil
		  while (not done) do
		  (setf done (not (search-forward-regexp "\\\\ref{\\(fig:.*\\)}" nil t)))
		  (if (> i 1000) (setf done t))
		  when (not done) do
		  (let ((m (match-string 1)))
			(if (not (member m refs))
				(push m refs)))
		  finally (return (reverse refs)))))


(defn collect-all-fig-labels []
  (save-excursion 
	(goto-char (point-min))
	(loop with done = nil and 
		  i = 0
		  while (not done) do
		  (setf done (not (search-forward-regexp "\\\\label{\\(fig:.*\\)}" nil t)))
		  (setf i (+ i 1))
		  (if (> i 1000) (setf done t))
		  when (not done)
		  collect (match-string 1))))

(defn insert-fig-by-number [n]
  (interactive "p")
  (let* ((lbls (collect-all-fig-labels))
		 (lbl (elt lbls (- n 1))))
	(insert (concat "\\ref{" lbl "}"))))

(defn insert-label []
  (interactive "")
  (let ((ref (completing-read "ref:" (collect-all-labels))))
	(insert (concat "\\ref{" ref "}"))))

(defn insert-label-from-file [from]
  (interactive "fwhich file?:")
  (let ((ref (completing-read "ref:" (with-current-buffer (find-file-noselect from) (collect-all-labels)))))
	(insert (concat "\\ref{" ref "}"))))


(defn get-bib-file []
  (save-excursion 
	(goto-char (point-min))
	(let ((found (search-forward-regexp "\\\\bibliography{\\(.*\\)}")))
	  (if (not found) (error "Can't find bib file.")
		(concat (match-string 1) ".bib")))))

(defn proper [cc]
  (list (car cc) (cdr cc)))

(defn testing []
  (dlet [[s e] (proper (bibtex-valid-entry))]
	(print (list s e))))

(defn char-at-point []
  (first (coerce (buffer-substring (point) (+ 1 (point))) 'list)))

(defn at-end? []
  (= (point) (point-max)))

(defn next-@ []
  (let ((start-point (point)))
	(forward-char 1)
	(loop with found = nil and
		  done = nil
		  while (not (or found done))
		  do
		  (cond 
		   ((at-end?)
			(setf done t)
			(setf found nil))
		   ((= ?@ (char-at-point))
			(setf done t)
			(setf found t))
		   (t 
			(forward-char 1)))
		  finally (return
				   (if found (point)
					 (progn (goto-char start-point) nil))))))

(defn next-{ []
  (let ((start-point (point)))
	(forward-char 1)
	(loop with found = nil and
		  done = nil
		  while (not (or found done))
		  do
		  (cond 
		   ((at-end?)
			(setf done t)
			(setf found nil))
		   ((= ?{ (char-at-point))
			(setf done t)
			(setf found t))
		   (t 
			(forward-char 1)))
		  finally (return
				   (if found (point)
					 (progn (goto-char start-point) nil))))))


(defn get-bibtex-entries [buffer]
  (interactive)
  (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-min))
	  (loop while (re-search-forward "@\\(.*\\)[ 
]*{\\(.*\\)," nil t) collect (match-string 2)))))

(lexical-let ((entries nil))
  (defn get-bibtex-entries-cached [filename]
	(with-current-buffer (get-buffer filename)
	  (if (or (buffer-modified-p) 
			  (eq nil entries))
		  (progn 
			(setf entries (get-bibtex-entries (get-buffer filename)))
			entries)
		entries))))



(defn insert-citation []
  (interactive "")
  (let ((bibbuf (find-file-noselect (get-bib-file))))
	(insert 
	 (concat "~\\cite{" 
 			 (completing-read "which: " (get-bibtex-entries (get-bib-file)))
			 "}"))))

(defn insert-citeps []
  (interactive "")
  (let ((bibbuf (find-file-noselect (get-bib-file))))
	(insert (loop with cites = () and
				  read = () while (progn (setf read 
											   (completing-read 
												(concat 
												 "which ('' terminates) {" 
												 (join cites ", ") 
												 "} :") 
												(get-bibtex-entries (get-bib-file))))
										 (not (or (string= "" read) (string= "_" read))))
				  collect read into cites
				  finally (return
						   (concat "\\citep{" 
								   (join cites ", ") 
								   "}"))))))

(defn insert-cite-keys []
  (interactive "")
  (let ((bibbuf (find-file-noselect (get-bib-file))))
	(insert (loop with cites = () and
				  read = () while (progn (setf read 
											   (completing-read 
												(concat 
												 "which ('' terminates) {" 
												 (join cites ", ") 
												 "} :") 
												(get-bibtex-entries (get-bib-file))))
										 (not (or (string= "" read) (string= "_" read))))
				  collect read into cites
				  finally (return
						   (concat "" 
								   (join cites ", ") 
								   ""))))))


(defn reformat-paragraphs [start end]
  (interactive "r") 
  (dlet [[start end] (sort* (list start end) #'<)]
	(print "This is slow, but works")
	(save-excursion 
	  (forward-char 1)
	  (loop with buflen = (point-max)
			while ($ (point) betweeni? start end) do
			(print "looping")
			(forward-paragraph 1)
			(backward-char 3)
			(fill-paragraph)
			(setf end (+ end (- (point-max) buflen)))
			(setf buflen (point-max))
			(forward-paragraph 1)))))

(dont-do 
 (domonad monad-seq
		  [part (split-string "(bob et al; ted and sandy; dogs)" "[;()]")
				part (list (chomp part))
				part (if (string= "" part) nil (list part))]
		  part))

(defn text->bib [ctn]
  (dlet [[raw-parts year] (mapcar #'chomp (split-string ctn "[,]"))
		 name (join (mapcar #'capitalize (split-string raw-parts "[ .]")) "")]
	(concat name year)))

(dont-do
 (text->bib "Markram et al, 2009"))

(defn texts->bibs [str]
  (dlet [bibs (domonad monad-seq
					   [part (split-string str "[;()]")
							 trimmed (list (chomp part))
							 text-citation (if 
											   (string= "" trimmed) nil
											 (list trimmed))
							 bib-citation (list (text->bib text-citation))]
					   bib-citation)]
	(concat "\\citep{" (join bibs ", ") "}")))

(defn replace-textual-citation [start end]
  (interactive "r")
  (let ((citep (texts->bibs (buffer-substring start end))))
	(kill-region start end)
	(insert citep)))

(defn goto-fig-n [n]
  (interactive "p")
  (goto-char (point-min))
  (loop for i from 1 to n do
		(re-search-forward (regexp-quote "\\begin{figure}")))
  (print (format "Figure %d" n)))

(defn next-fig []
  (interactive "")
  (re-search-forward (regexp-quote "\\begin{figure}")))

(dont-do
 (insert (texts->bibs "(Markram et al, 2009; James, 
1492; Mainend and Sejnowsky, 1996)")))

(defn goto-first [start end]
  (interactive "r")
  (let ((ss (buffer-substring-no-properties start end)))
	(with-current-buffer (get-buffer "dissertation.tex")
	  (search-forward-regexp (regexp-quote ss) nil t))))


(defn insert-latex-block [block-name]
  (interactive "s")
  (let ((start (point)))
	(insertf "\\begin{%s}\n" block-name)
	(let ((inside (point)))
	  (insertf "\n\\end{%s}\n" block-name)
	  (let ((end (point)))
		(goto-char inside)
		(indent-region start end)))))

(provide 'latex-utils)