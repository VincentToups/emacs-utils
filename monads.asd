;;;; monads.asd

(asdf:defsystem #:nil
  :serial t
  :depends-on (#:lisp-unit
               #:shadchen)
  :components ((:file "package")
               (:file "nil")))

