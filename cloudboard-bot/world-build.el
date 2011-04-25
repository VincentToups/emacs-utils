(require 'sets)
(require 'functional)
(require 'monads)
(require 'recur)
(require 'with-stack)
(require 'stack-words)

(setq *current-root* (tbl! :things (make-set nil) :relations (alist>>)))
(setq *focus-stack* (list *current-root*))
(setq *undo-stack* nil)

(defun focus-on (thing)
  (push thing *focus-stack*))

(defun done ()
  (pop thing *focus-stack*))

(defun thing-exists? (thing)
  ($ thing in-set (tbl *current-root* :things)))

(defun make-thing (thing)
  (if (not (thing-exists? thing))
	  (tbl! *current-root* :things
			(add-to-set (tbl *current-root* :things) thing))))
(defun relation-exists? (relation)
  (alist (tbl *current-root* :relations) relation))
(defun create-relation (relation)
  (if (not (relation-exists? relation))
	  (tbl! *current-root* :relations
			(alist>> (tbl *current-root* :relations)
					 :relation nil))))
(defun make-location-raw (thing location)
  (make-thing thing)
  (relate thing :is :location)
  (relate thing :at location))

(defun anything-at? (loc)
  (query-relation :at (par #'equal loc)))

(defun make-location (thing location)
  (let-if things (anything-at? location)
	  (format "Something(s) already located at %s: %S" location things)
	  (make-location-raw thing location)))
(defun add-to-location (thing location)
  (make-location-raw thing location))

(defun replace-location (thing location)
  (make-location-raw thing location))

(defun location-of (thing)
  (car (what-relates :at thing)))

(defun relate (thing1 how thing2)
  (if (not (relation-exists? how))
	  (create-relation how))
  (tbl! *current-root* :relations
		(alist-cons (tbl *current-root* :relations) how
					(list thing1 thing2))))

(defun unrelate (thing1 how thing2)
  (if (not (relation-exists? how))
	  (create-relation how))
  (tbl! *current-root* :relations
		(alist-remove-from-list (tbl *current-root* :relations) how
								(list thing1 thing2))))

(defun get-relation (relation)
  (alist (tbl *current-root* :relations) relation))

(defun query-relation (relation fun)
  (mlet* monad-maybe^i
		 ((relation (get-relation relation)))
		 (mapcar #'car (filter (lambda (sub-rel)
								 (funcall fun (cadr sub-rel))) relation))))

(defun what-relates (relation object)
  (mlet* monad-maybe^i 
		 ((relation (get-relation relation))
		  (reduced 
		   (filter&map 
			(lambda (rel)
			  (eq (car rel) object))
			#'cadr
			relation)))
		 reduced))

(defun rel-get (relation object)
  (mlet* monad-maybe^i 
		 ((relation (get-relation relation))
		  (reduced 
		   (filter&map 
			(lambda (rel)
			  (eq (car rel) object))
			#'cadr
			relation)))
		 reduced))

(setq *directions* (list :north :north-east :east :south-east :south :south-west :west :north-west))
(word: string-tail {: str -- str-tail :}
	   dup length 1 swap 3>substring)
(word: string-head {: str -- head :}
	   0 1 3>substring)

(word: kw-string {: kw -- str :}
	   "%s" swap 2>format string-tail)
(defun abbreviate-direction (dir)
  (||| {dir} kw-string 
	   "-" rxq split-string 
	   { string-head } map 
	   "" 2>join 1>make-keyword))

(defun dir+ (d1 d2)
  (mapcar* #'+ d1 d2))
(defun dir- (d1 d2) 
  (mapcar* #'- d1 d2))

(setq 
 north '(0 1)
 north-east '(1 1)
 east '(1 0)
 south-east '(1 -1)
 south '(0 -1)
 south-west '(-1 -1)
 west '(-1 0)
 north-west '(-1 1))


(defun north (dir) (dir+ dir north))
(defun north-east (dir) (dir+ dir north-east))
(defun east (dir) (dir+ dir east))
(defun south-east (dir) (dir+ dir south-east))
(defun south (dir) (dir+ dir south))
(defun south-west (dir) (dir+ dir south-west))
(defun west (dir) (dir+ dir west))
(defun north-west (dir) (dir+ dir north-west))

(univalent-stack-words 
 north north-east east 
 south-east south south-east
 west north-west)

(defun move (from-loc &rest dirs)
  (reduce 
   (lambda (loc dir)
	 (cond 
	  ((functionp dir) (funcall dir loc))
	  ((listp dir) (dir+ dir loc))))
   dirs 
   :initial-value from-loc))

(word: from {: pos motion -- position :}
  cons 'move swap 2>apply )
(word: make-location {: position location -- :}
  swap 2>make-location)
(word: add-to-location {: position location -- :}
	   swap 2>add-to-location)

(word: location {: place -- location :}
	   1>location-of )

(|||p '(0 0) :castle make-location)
(|||p '(0 0) :mote add-to-location)
(|||p :castle location north west :entrance-to-mountains make-location)
(|||p :castle location)
(|||p '(0 0) {/ north north north west west /} from )
(|||p :entrance-to-mountains location)
(get-relation :at)
(query-relation :at (par #'equal '(0 0)))

(defun whats-at (position)
  (query-relation :at (par #'equal '(0 0))))

(make-location-raw :twizted-castle '(0 0))

(relate "c" :sigil-of :twizted-castle)
(get-relation :sigil-of)

(make-location :dog '(0 0))
(replace-location :swamp '(0 0))
(make-thing :karlor)
(relate :karlor :is :player)


(relate :karlor :stats-of '((str 10) (ag 11) (wis 8)))

(rel-get :stats-of :karlor)


(defun loc-min-max-reduction (min-max pt)
  (let-seq (min max) min-max 
		   (list
			(mapcar* #'min min pt)
			(mapcar* #'max max pt))))

(defun loc-min-max (pts)
  (reduce #'loc-min-max-reduction
		  pts
		  :initial-value
		  (list (car pts) (car pts))))
(defun draw-map-row (row min-x max-x)
  (loop for i from min-x to max-x append
		(if (query-relation 


(defun draw-map ()
  (let* ((at (get-relation :at))
		 (locations (mapcar #'cadr at)))
	(let-seq 
	 (min max) (loc-min-max locations)
	 (






(unrelate :karlor :is :player)
(what-relates :is :karlor)