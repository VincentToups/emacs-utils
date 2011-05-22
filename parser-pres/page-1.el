;;; Monadic Parser Combinators
;;; A Ground up Introduction

;; The best way, I think, to understand how these things works is to
;; consider the question of what a monadic parser combinator is in
;; the following order:

;;     1) What is our representation of a parser?
;;     2) How do we combine them?
;;     3) How does this combination strategy form a monad?

;; Depending on your temperament, you might not even care about 3,
;; which is fine.  The parser monad is useful without worrying too
;; hard about how monads work in general, but we will try to make
;; that clear in the course of the presentation.

;; N.B. : As with my parser-monad library, the general shape of this
;; talk owes a great deal to Drew Crampsie's SMUG Monadic Parser
;; Combinator Library for Common Lisp.
;; 
;; I've simplified the parsers covered by this monad substantially for
;; the purposes of clarity and brevity, however.  At the end of the
;; talk we'll touch briefly on the nature of those simplifications. 

(require 'el-pres)
(rebuild-control-panel)
   
;;;Controls Home    . >>>   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index