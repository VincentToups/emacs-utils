(provide 'skip-lists)
(require 'cl)

(defstruct sl-node 
  (val)
  (links))

(defun make-participation (max div)
  