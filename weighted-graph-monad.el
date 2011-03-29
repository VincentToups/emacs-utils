(require 'utils)
(require 'monads)
(require 'functional)
(require 'cl)
(require 'advanced-utils)
(provide 'weighted-graph-monad)
(provide 'recur)

(defun* empty-weighted-graph (&key (symmetric t) (pred #'equal))
  "Create an empty weighted-graph.  Not usually called directly."
  (alist>> :nodes nil ; the nodes
		   :connections nil ; the edges
		   :symmetric symmetric ; whether edges should be symmetric or go both ways.
		   :pred pred)) ; the predicate to test for node equality.

(defun wg-add-node (wg node)
  "Add a NODE to a weighted graph WG."
  (alist-add-to-set wg :nodes node (alist wg :pred)))

(defun wg-pair-to-canonical-order (wg node1 node2)
  "Because nodes need not be sortable, this function puts a pair
of nodes into Canonical Order based on the nodes already in the
weighted graph WG.  Canonical order is the order in which
the nodes appear in the node list."
  (sort (list node1 node2)
		(decorate-all #'<
					  (lambda (it)
						(length (member-if 
								 (par (alist wg :pred) it)
								 (alist wg :nodes)))))))

(defvar *link-table* (tbl!))
(defun symmetric-link (node1 node2)
  (let ((lnkn (tbl-or *link-table* (list node1 node2) nil))
		(lnku (tbl-or *link-table* (list node2 node1) nil)))
	(if (or lnkn lnku) lnkn
	  (progn 
		(tbl! *link-table* (list node1 node2) (list node1 node2))
		(tbl! *link-table* (list node2 node1) (list node1 node2))
		(list node1 node2)))))

(defun wg-add-connection (wg node1 node2 connection)
  "Add a connection to the weighted graph WG between NODE1 and
NODE2 of weight CONNECTION.  If the nodes are not already in the
graph, add them before creating the connection.

If the connection is already in the graph, CONNECTION is
added to the connection strength.

When the graph is symmetric, the connection between NODE1 and
NODE2 is the same as that between NODE2 and NODE1.  
"
  (let* ((wg (alist-add-to-set wg :nodes node1))
		 (wg (alist-add-to-set wg :nodes node2))
		 (c-nodes (symmetric-link node1 node2))
		 (connections (alist wg :connections)))
	(let ((out 
		   (if connection
			   (alist>> wg :connections
						(pred-alist-conjugate (alist wg :pred) connections c-nodes
											  (lambda (old)
												(+ old connection))
											  0))
			 (alist>> wg :connections
					  (pred-dissoc (alist wg :pred) connections c-nodes)))))
	  out)))

(defun weighted-graph (nodes &rest connections)
  "Create a weighted graph with NODES and optional CONNECTIONS.
Connections should be a series of PAIR VAL pairs.  Connections are
added with WG-ADD-CONNECTION semantics, and so new nodes will be 
created as needed."
  (let ((wg (reduce #'wg-add-node nodes
					:initial-value (empty-weighted-graph)))
		(connections (bunch-list connections)))
	(reduce 
	 (lambda (wg connection)
	   (wg-add-connection wg (car (car connection))
						  (cadr (car connection))
						  (cadr connection)))
	 connections
	 :initial-value wg)))

(defun* wg (&key (nodes nil) (connections nil)
				 (symmetric t) (pred #'equal))
  "WG creates a weighted graph with a key/val interface.  NODES
is a list of nodes.  CONNECTIONS is a flat list of triples
describing connections as [N1 N2 STREN].  

SYMMETRIC indicates with T/NIL symmetry.  
PRED is the equality predicate for NODES."
  (let* ((nodes (unique nodes pred))
		 (wg (alist>> (empty-weighted-graph :pred pred :symmetric symmetric)
					  :nodes nodes)))
	(reduce 
	 (lambda (wg conn)
	   (apply #'wg-add-connection wg conn))
	 (bunch-by 3 connections)
	 :initial-value wg)))

(defun wg-combine-nodes (wg1 wg2)
  (alist>> wg1 :nodes 
		   (unique (append (alist wg1 :nodes)
						   (alist wg2 :nodes))
				   (alist wg1 :pred))))

(example
 (wg-combine-nodes 
  (weighted-graph '(:a :b :c))
  (weighted-graph '(:d :e))))

(defun wg-combine2 (wg1 wg2)
  "Combine weighted graphs WG1 and WG2 using the symmetry
semantics of WG1 by adding the connections in WG2 to WG1.  This
is half of the weighted graph BIND operation."
  (let* ((wg1 (reduce 
			   (lambda (wg conn)
				 (wg-add-connection wg (car (car conn))
									(cadr (car conn))
									(cadr conn)))
			   (alist wg2 :connections)
			   :initial-value
			   (wg-combine-nodes wg1 wg2))))
	wg1))

(defun wg-combine (&rest args)
  "Combine weighted graphs in ARGS using the symmetry
semantics of (car ARGS) using REDUCTION with WG-COMBINE2"
  (cond ((not args) (empty-weighted-graph))
		(t (reduce #'wg-combine2 
				   args))))

(example 
 (require 'weighted-graph-monad)
 (cl-prettyprint (wg-combine (weighted-graph '(:a :b :c) '(:a :b) 2)
							 (weighted-graph '(:d :e)    '(:d :e) 3)))

 )

(defun weighted-graph-bind (v f)
  "Bind the value V, a weighted graph, with the function F,
which should take a node value and return a weighted graph 
to be combined with V."
  (reduce 
   (lambda (wg node)
	 (let ((sub-graph (funcall f node)))
	   (wg-combine wg sub-graph)))
   (alist v :nodes)
   :initial-value (alist>> v :symmetric t)))


(defun asymmetric-weighted-graph-bind (v f)
  "Bind the value V, a weighted graph, with the function F,
which should take a node value and return a weighted graph 
to be combined with V."
  (reduce 
   (lambda (wg node)
	 (wg-combine wg (funcall f node)))
   (alist v :nodes)
   :initial-value (alist>> v :symmetric nil)))


(defun wg-return (&optional item)
  "Monadic return operation for weighted graphcs.  Returns a
weighted graph with one node ITEM and no connections.  If ITEM
is omitted or NIL, return an empty weighted graph."
  (if item
	  (weighted-graph (list item))
	(weighted-graph nil)))

(defun asymmetric-wg-return (&optional item)
  "Asymmetric weighted graph return function."
  (if item 
	  (alist>> (weighted-graph (list item)) :symmetric nil)
	(empty-weighted-graph :symmetric nil)))

(defvar weighted-graph-monad 
  (tbl! :m-bind #'weighted-graph-bind
		:m-return #'wg-return)
  "The weighted graph monad.")

(defvar asymmetric-weighted-graph-monad 
  (tbl! :m-bind #'asymmetric-weighted-graph-bind
		:m-return #'asymmetric-wg-return)
  "The assymetric weighted graph monad.")

(example 
 (require 'weighted-graph-monad)
 (cl-prettyprint (domonad weighted-graph-monad
						  [node1 (weighted-graph '(a b c))
								 node2 (weighted-graph '(a b c))
								 graph (wg-connect node1 node2 1)]
						  graph))

 )

(defun nodes (&rest nodes)
  "Create a weighted graph with only NODES."
  (wg :nodes nodes))

(defun node (node)
  "Create a graph with a single node."
  (wg :nodes (list node)))

(defun edges (&rest edges)
  "Create a weighted graph with EDGES. Nodes are added as needed,
repeated edges are combined.  EDGES should be a flat list of
triples of the form NODE1 NODE2 STREN."
  (wg :connections edges))

(defun asymmetric-edges (&rest edges)
  "Create a graph containing the asymmetric EDGES."
  (wg :symmetric nil :connections edges))

(defun edge (from to stren)
  "Create a graph with a single edge FROM to TO with strength STREN."
  (wg :connections (list from to stren)))

(defun asymmetric-edge (from to stren)
  "Create a graph with a single asymmetric edge FROM to TO with strength STREN."
  (wg :symmetric nil :connections (list from to stren)))

(defun unedge (from to)
  "Specify a weighted graph which explicitely doesn't contain an edge FROM TO."
  (alist>> (empty-weighted-graph) :connections `(((,from ,to) nil))))

(defun asymmetric-unedge (from to)
  "Specify a weighted graph which explicitely doesn't contain an edge FROM TO."
  (alist>> (empty-weighted-graph) :symmetric nil :connections `(((,from ,to) nil))))

(defun wg-connect (from to mag)
  "Return a weighted-graph with a connection between FROM and TO of magnitude MAG."
  (weighted-graph (list from to) (list from to) mag))

(defun wg-disconnect (from to)
  "Removes a connection from a weighted graph."
  (alist>> (empty-weighted-graph) :connections `(((,from ,to) nil))))

(example 
 (require 'weighted-graph-monad)

 (cl-prettyprint 
  (domonad weighted-graph-monad 
		   [node1 (nodes 'a 'b 'c)
				  node2 (nodes 'a 'b 'c)
				  final-graph
				  (if (equal node1 node2) (empty-weighted-graph)
					(edge node1 node2 1))]
		   final-graph))
 ((:connections (((c a) 1) ((b c) 1) ((a b) 1)))
  (:nodes (a b c))
  (:symmetric t)
  (:pred equal))


 "This example builds a network of sexual contact.  Men and women
are network nodes and we use the weighted graph monad to add
connections between men and women indicating the number of
encounters they may have had."

 (cl-prettyprint (labels 
					 ((man (name) (list (intern "man") name))
					  (woman (name) (list (intern "woman") name))
					  (man? (thing)
							(and (listp thing)
								 (eq (car thing) (intern "man"))))
					  (two-men? (thing1 thing2)
								(and (man? thing1)
									 (man? thing2)))
					  (two-women? (thing1 thing2)
								  (and (woman? thing1)
									   (woman? thing2)))
					  (woman? (thing)
							  (and (listp thing) 
								   (eq (car thing) (intern "woman")))))
				   (let ((hetero-prob .3) ;set the probability of heterosexual encounters
						 (homo-prob .1) ;set the probability of homosexual encounters
						 (empty-graph (weighted-graph nil)) ; we will be using the empty graph a lot
										; this is m-zero anyway.
						 (population ; describe the population as a list of nodes, we will fill in 
										; connections later.
						  (weighted-graph 
						   (list
							(man :bob)
							(man :ted)
							(man :leo)
							(man :theo)
							(man :jon)
							(man :herbert)
							(woman :sally)
							(woman :alice)
							(woman :jane)
							(woman :elizabeth)
							(woman :karen)))))
					 (domonad weighted-graph-monad 
							  [person1 population
									   person2 population
									   connected-pop
									   (cond
										((equal person1 person2) empty-graph) ; lol, people don't have sex with themselves
										((or (two-men? person1 person2)
											 (two-women? person1 person2))
										 (if ($ (random* 1.0) < homo-prob) ; ie, there is a homosexual encounter
											 (wg-connect person1 person2 1)
										   empty-graph))
										(t 
										 (if ($ (random* 1.0) < hetero-prob) ; ie, there is a heterosexual encounter
											 (wg-connect person1 person2 1)
										   empty-graph)))]
										; return the final connected population after one round of activity.
							  connected-pop))))
((:connections ((((man :bob)
				  (woman :karen))
				 1)
				(((man :bob)
				  (woman :elizabeth))
				 1)
				(((man :herbert)
				  (man :bob))
				 1)
				(((man :ted)
				  (man :herbert))
				 1)
				(((man :leo)
				  (woman :karen))
				 1)
				(((man :leo)
				  (woman :sally))
				 1)
				(((man :theo)
				  (woman :elizabeth))
				 1)
				(((man :theo)
				  (man :jon))
				 1)
				(((man :herbert)
				  (woman :elizabeth))
				 1)
				(((man :herbert)
				  (man :theo))
				 1)
				(((woman :sally)
				  (woman :karen))
				 1)
				(((woman :sally)
				  (man :jon))
				 1)
				(((woman :alice)
				  (woman :sally))
				 1)
				(((woman :alice)
				  (man :theo))
				 1)
				(((woman :alice)
				  (man :bob))
				 1)
				(((woman :jane)
				  (man :theo))
				 1)
				(((woman :jane)
				  (man :bob))
				 1)
				(((woman :elizabeth)
				  (man :leo))
				 1)
				(((woman :karen)
				  (man :jon))
				 1)))
 (:nodes ((woman :karen)
		  (woman :elizabeth)
		  (woman :jane)
		  (woman :alice)
		  (woman :sally)
		  (man :herbert)
		  (man :jon)
		  (man :theo)
		  (man :leo)
		  (man :ted)
		  (man :bob)))
 (:symmetric t)
 (:pred equal))


 )


