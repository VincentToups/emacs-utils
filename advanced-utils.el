(require 'utils)
(require 'with-stack)
(require 'defn)
(require 'recur)


(defun file-location (filestr)
  (||| {filestr} "/" rxq split-string reverse cdr reverse "/" join "/" 2>concat))

(defun file-name (filestr)
  (||| {filestr} "/" rxq split-string reverse car))

(defun file-extension (filestr)
  (||| {filestr} 1>file-name "." rxq split-string reverse car))

(dont-do 
 (file-location "/this/is/a/test/press")
 (file-name "/this/is/a/test/press")
 (file-extension "/this/is/a/test/press.txt")
)

(nthcdr 3 '(a b c d))

(recur-defun* bunch-by (n input &optional (output nil))
  (if input
	  (let ((bunch (elts input (range n)))
			(rest (nthcdr n input)))
		(recur n rest (cons bunch output)))
	(reverse output)))


(provide 'advanced-utils)