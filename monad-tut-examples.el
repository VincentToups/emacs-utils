(require 'monads)
(require 'functional)
(require 'utils)


(defvar *people* '(:ted :lea :leo :james :harvey :sally :jane :andrew :catherine) "A list of all the people that matter.")
(defvar *friends-db* 
	  '((:ted (:lea :leo :sally :andrew :catherine :leo :jane))
		(:lea (:ted :leo :jane :andrew :harvey :sally :catherine))
		(:leo (:ted :lea :ted :harvey :sally :jane :andrew :catherine
					:harvey :andrew :catherine))
		(:james (:jane :harvey :jane))
		(:harvey (:leo :lea :leo :james :harvey :harvey :sally))
		(:sally (:ted :leo :lea :harvey :jane :andrew))
		(:jane (:lea :leo :james :sally :ted :james :andrew :catherine))
		(:andrew (:ted :lea :leo :sally :jane :leo))
		(:catherine (:ted :leo :lea :leo :jane :catherine :catherin))) "Our database of friend connections.")

(defun friends-of (person) 
  "Return a list of all the people in friends-db."
  (alist *friends-db* person)
										;alist is a function which retrieves
										;a key's data from an association list.
)



