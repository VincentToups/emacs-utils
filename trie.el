(provide 'trie)
(require 'utils)

(defun trie-children? (trie)
  (cdr trie))
(defun trie-key (trie)
  (car (car trie)))
(defun trie-value (trie)
  (cdr (car trie)))
(defun trie-left (trie)
  (car (cdr trie)))
(defun trie-right (trie)
  (cdr (cdr trie)))

(defun trie-node (key val left right)
  (cons (cons key val) (cons left right)))

(defun retrieve- (trie key)
  (cond ((not trie) nil)
		((not key)  nil)
		(otherwise
		 (let ((key-key (car key))
			   (children (trie-children? trie)))
		   (cond 
			((and (eq key-key (car trie))
				  (not trie-children?))
			 (trie-value trie)))))))

(cleave '(min max) (coerce "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890~!@#$%^&*()_+=-`\"':;.,><?/\\{}[]|" 'list))

(defun retrieve (trie key)
