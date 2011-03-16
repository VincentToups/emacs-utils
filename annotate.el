(require 'utils)
(require 'cl)
(require 'functional)
(require 'scripting)

(defvar *annotation-directory*
  "~/.emacs.d/annotations/")



(defun set-annotation-directory (dir)
  (setq *annotation-directory* dir)
  (unless (directoryp dir)
	(unless (file-exists-p dir)
	  (make-directory dir))))

(defun gen-note-file-data (link-text)
  (let* ((hash (md5 link-text))
		 (rpieces (reverse (mapcar (par #'coerce 'string) (bunch-list (coerce hash 'list)))))
		 (leaf (car rpieces))
		 (stem (join (reverse (cdr rpieces)) "/")))
	(list leaf (dircat *annotation-directory* stem))))

(defun note-file-data->name (data)
  (concat (dircat (cadr data) (car data)) ".md"))

(note-file-data->name (gen-note-file-data "test"))