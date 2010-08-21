(require 'utils)
(require 'with-stack)
(require 'defn)


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

(provide 'advanced-utils)