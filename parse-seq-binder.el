;; parse-seq-binder
;; parses a sequence binder for clojure-like binding
;; See defn.el

(require 'cl)
(require 'utils)

(defvar currently-defining-defn 'lambda)
; defvar this so that we can satisfy the elisp compiler.

(defun parse-and-check-seq-binder (binder)
  "Given a BINDER expression describing a SEQUENCE, check and parse the expression into a useful form.
Returns a list of the form:
 ( BINDERS
   REST-EXPRESSION
   AS-SYM
   OR-FORM )

 Binders constitutes the 'ordinary' variable binding expressions.
 REST-FORM is the symbol to associate with anything after an '&'
 token.  AS-SYM is the symbol to bind the entire expression to.
 NIL signifies NONE.  OR-FORM is the expression (if any) to
 destructuring if destructuring the input fails.
   "
  (let-tbl
   ((binders :binders)
	(as-sym  :as-sym)
	(rest-form :rest-form)
	(or-form :or-form))
   (foldl 
	(lambda (it ac)
	  (let-tbl
	   ((i         :i)
		(prev      :prev)
		(state     :state)
		(n-as      :n-as)
		(n-or      :n-or)
		(n-rest    :n-rest)
		(as-sym    :as-sym)
		(or-form   :or-form)
		(rest-form :rest-form)
		(binders :binders)) ac
		(case state
		  (:parsing-binders
		   (parse-seq-binders it ac))
		  (:parsing-rest
		   (tbl! ac
				 :state     :parsing-special-forms
				 :rest-form it
				 :prev it
				 :i         1))
		  (:parsing-special-forms
		   (parse-seq-special-forms it ac)))))
	(tbl!
	 :i       0
	 :state   :parsing-binders
	 :n-as    0
	 :n-or    0
	 :n-rest  0
	 :rest-form nil
	 :as-sym  nil
	 :or-form nil
	 :binders '())
	(vector->list binder))
   (list binders rest-form as-sym or-form)))

(defun parse-seq-special-forms (it ac)
  "Parsing function for the special forms part of a SEQ binding
expression.  Takes a table in AC representing the parser state,
and returns an appropriately modified table.  
IT is the current token."
  (let-tbl 
   ((i :i)
	(state :state)
	(n-as :n-as)
	(n-or :n-or)
	(as-sym :as-sym)
	(or-form :or-form)
	(binders :binders)
	(prev :prev)) ac
	(cond 
	 ((oddp i)
	  (if (or
		   (eq :as it)
		   (eq :or it))
		  (let* ((count-key (case it (:as :n-as) (:or :n-or)))
				 (n-special-form (+ 1 (tbl ac count-key))))
			(if (> n-special-form 1) (error "More than one %s clause in table binder in %s." it currently-defining-defn))
			(tbl! ac
				  :prev it
				  :i (+ i 1)
				  count-key n-special-form))
		(error "Unrecognized special form keyword %s in %s" it currently-defining-defn)))
	 ((evenp i)
	  (let ((spec-key (case prev (:as :as-sym) (:or :or-form))))
		(case prev
		  (:as 
		   (if (symbolp it)
			   (tbl! ac
					 :i (+ i 1)
					 :prev it
					 spec-key it)
			 (error "As forms must be symbols.  Got %s instead in %s" it currently-defining-defn)))
		  (:or 
		   (tbl! ac
				 :i (+ i 1)
				 :prev it
				 spec-key it))))))))

(defun parse-seq-binders (it ac)
  "Parser for the sequential part of a SEQ binder.  Takes a table
representing the parser state in AC, and the current token IT,
and returns an appropriately modified table."
  (let-tbl
   ((i       :i)
	(prev    :prev)
	(state   :state)
	(n-as    :n-as)
	(n-or    :n-or)
	(n-rest  :n-rest)
	(as-sym  :as-sym)
	(or-form :or-form)
	(binders :binders)) ac
   (cond
	((eq it '&)
	 (let ((n-rest (+ n-rest 1)))
	   (if (> n-rest 1) (error "More than one rest (&) clause detected parsing seq binder in %s." currently-defining-defn)
		 (tbl! ac
			   :state :parsing-rest
			   :prev it
			   :n-rest n-rest
			   :i (+ i 1)))))
	((keywordp it)
	 (if (or (eq :as it)
			 (eq :or it))
		 (let* ((count-key (case it (:as :n-as) (:or :n-or)))
			    (n-special-form (+ 1 (tbl ac count-key))))
		   (if (> n-special-form 1)
			   (error "More than one %s special-form in %s." it currently-defining-defn)
			 (tbl! ac
				   :i     2
				   count-key n-special-form
				   :state :parsing-special-forms
				   :prev  it)))
	   (error "Unrecognized special form %s in %s " it currently-defining-defn)))
	(t
	 (tbl! ac
		   :binders (suffix binders it)
		   :i       (+ i 1)
		   :prev it)))))



(comment
 (parse-and-check-seq-binder [a b c & rest :as x :or (list 1 2 3)]))

(provide 'parse-seq-binder)
	   
	
