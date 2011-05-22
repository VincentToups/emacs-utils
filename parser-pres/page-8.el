;;; A bit more about bind.

(find-file-other-frame "~/work/art/monadic-types-of-interest.png")
(find-file-other-frame "~/work/art/bind.png")

;;; * Bind is kind of unintuitive.
;;; * However, it is more useful than "combine" because 
;;;   it facilitates sequencing.
;;; * bind's second argument is a lambda
;;; * a lambda is a delayed computation which depends on 
;;;   _unbound_ values.
;;; * bind _binds_ these values in an ordered way, facilitating
;;;   the sequencing of computations which result in monadic 
;;;   values.

;;; In the parser monad:
;;; * each lambda is a "delayed computation" which results in a 
;;;   _new parser_ when it is called with the value produced
;;;   by a previous parser.
;;; * bind combines the new parser with the old parser, 
;;;   handling the plumbing needed to connect them together.
;;; * this plumbing is
;;;    - check for nil
;;;    - wrap up everything in a containing parser.


;;;Controls Home   <<< . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index