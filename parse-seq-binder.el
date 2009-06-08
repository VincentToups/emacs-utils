;; parse-seq-binder
;; parses a sequence binder for clojure-like binding

(require 'cl)
(require 'utils)

(setq currently-defining-defn 'lambda)

(defun parse-seq-special-forms (it ac)
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

(defun parse-and-check-seq-binder (binder)
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

(comment
 (parse-and-check-seq-binder [a b c & rest :as x :or (list 1 2 3)]))

(provide 'parse-seq-binder)
	   
	
