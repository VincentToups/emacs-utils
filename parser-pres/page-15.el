;;; Conclusions/Observations

;;; * Monadic Parser Combinators are about combining simple parsers
;;; * Like most monads, you need special syntax to make best use of
;;;   them.

;;; * Since parsers are functions, they live in your language, can be
;;;   tested individually, and can use the full power of the
;;;   underlying language.  Incremental development and testing makes
;;;   writing parsers the same as writing any other program.

;;; Future Thoughts: 

;;; * The parser monad isn't really anything special.  It is actually
;;;   the monad you get by transforming the maybe monad with the state
;;;   monad transformer.

;;; * Ergo, (state-t sequence-m) is the non-deterministic parser monad
;;;   in which individual parsers may return multiple results.

;;; * And (state-t stream-m) is the lazy, non-deterministic parser
;;;   monad.  Parsers in this monad return a stream (possibly
;;;   infinite) of results.

;;; * What about _additional_ state?  Consider the monad of functions:
;;;   (lambda (&rest states)
;;;     ;;; does things
;;;     (pair a-result states))
;;;
;;;   By packing a stack or two into states one could parse with precedence. 


;;;Controls Home   <<< .    1   2   3   4   5   6   7   8   9   10   11   12   13   14   15   
;;;         Index