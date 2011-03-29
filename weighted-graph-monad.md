The Weighted Graph Monad
========================

This document is a monad tutorial/cry for help.  It first goes over
monads in general, as far as I can say I understand them.  Then it
introduces a "novel" monad which I haven't seen described elsewhere on
the internet.  I'm certain that this "weighted graph monad" is not my
invention, because, as I understand it, any data structure can be
associated with any number of monads.  I think I understand monads
well enough at this point that this document will be useful to people
who come from a lisp background as far as getting a deeper
understanding of some of the things about them.  If that is you, you
can focus on the first section.

N.B.: Lispers who are interested in monads should read Drew Crampsie's
write up on his SMUG library located
[here](http://common-lisp.net/~dcrampsie/smug.html) and Jim Duey's
amazingly excellent [monad tutorial for
Clojure](http://intensivesystems.net/writings.html), which should
never the less be useful to a generalized lisper.  Although mostly
directed at OCaml, I also found Chris Barker and Jim Pryor's course
notes extremely enlightening.  They can apparently be found
[here](http://lambda.jimpryor.net/), though the URL implies this might
be a link which becomes stale.

The second section is aimed at people who understand monads better
than I do.  In it, I describe the weighted graph monad as I have
implemented it.  Although it seems like a reasonable way to do things,
its not clear that this is the most "monady" way of implementing such
a monad.  People seeking a better understanding of monads are, of
course, welcome to read the second part.  I think even if it is
missing some grace and elegance, it is probably useful to see a monad
built up from nothing, as it were.  However, bearing the above
qualifications in mind, the links above will prove much more useful to
you than this document.  Comments about the weighted graph monad or
the tutorial can be directed to vincent dot toups at gmail's domain.
I'd be really delighted if someone more knowledgable could shed light
on the graph monad.

Monads
------

A monad is defined by at least two functions.  Bind takes values out
of a monadic value, and applies a function to those values.  We don't
worry too much about types in emacs lisp, but its useful to consider
them here, because bind doesn't just apply ANY function to the value
inside the monad, and it can't return just any old type.  Bind takes a
monadic value and a function which accepts a value of the kind stored
_in_ the monad and which returns a monadic value again.

The other function which defines a monad is the monadic Return
function.  This is a function which takese a value of the kind stored
in the monad and returns the simplest monadic value possible that
retains the input value.  Return is almost the simplest possible
function which can be passed to Bind - it takese a "naked" value, and
returns a simple monadic value, just like any _monadic function_
must.

Some monads define a value called the Monadic Zero, which is sort of
the simplest possible monadic value for a given monad.  It is called a
zero by analogy to multiplication because binding any function to the
monadic zero returns the monadic zero.  So another simple function
which we could pass to bind takes a value, ignores it, and returns the
monadic zero.

People always bring up the sequence monad here, and although it is a
clear and simple example of a monad, I don't recall it being
particularly enlightening.  But lets go through it anyway.  In the
sequence monad, Sequences are the monadic values, and the monadic
functions are those functions which take an object of the kind stored
in the sequence and return a new sequence.  If we are dealing with
lisp data, the sequence monadic values are lists of lisp data.  And
the sequence monadic functions are functions which take some kind of
lisp data and return a list.

Values we can stick "in" the monad:

    23
    "test"
    (list 1 2 3 4)
    'x
    :z

Monadic values:

    (list 1 2 3 4)
    (list :x (list 1 2 3 4) "test")
    (list 'x 'y 'z)

Monadic functions:

    (lambda (x) nil) ; nil is the smallest list
    (lambda (x) (list x x))
    (lambda (x) (list x (+ x 1) (- x 1)))
    (lambda (x) (list 1 2 3 4 5))

The bind operation for the sequence monad is simple.


    (defun seq-bind (v f)
      (let ((monadic-values (mapcar f v)))
        (apply #'append monadic-values)))

We know we can map f over v because v is a sequence.  We know we can
apply append to the results because f is a monadic function which
always returns lists.  If we misbehave, and pass an f that doesn't
return lists in, we'll get a run time error.  Similarly if v isn't a
list.

N.B. - seq-bind is often called `mapcat` for "map and concatenate".

The sequence return function is even simpler.  It is just the `list`
function.  `List` is obviously a monadic function - it takes an item
and returns a list.  

Using these functions we can write a function which takes a list, and
returns a list where every element is repeated twice.

   (defun dup (x) (list x x)) ; return a list made of two x's
   (defun dup-elements (lst)
     (seq-bind lst #'dup))

   (dup-elements '(1 2 3 4)) -> (1 1 2 2 3 3 4 4)

Note that dup-elements is _not_ a monadic function.  It takes _a list_
as input.  You can think of the monad as being an interface layer
between things and lists, a contract that guarantees that for any
monadic function it will do something right with the output.  Given
that dup-elements is not a monadic function, what do we do if we want
to use the sequence monad to dup the elements twice?  One possibility
is:

    (defun double-dup (lst)
      (seq-bind (seq-bind lst #'dup) #'dup))
    (double-dup '(1 2 3) -> '(1 1 1 1 2 2 2 2 3 3 3 3)

It is now time to take two birds with one stone.  We'd like to be able
to succinctly chain monadic functions, sometimes without even giving
them explicit names.  We do this with `do notation` in Haskell.  In
Lisp we can roll our own macro, `domonad` (based on the Clojure monad
library). 

   (require 'monads)
   (defun double-dup (lst)
     (domonad monad-seq [x lst
                         y (dup x)
                         z (dup y)]
       z))

This body of double-dup is equivalent to the expression:

    (seq-bind lst 
    (lambda (x)
      (seq-bind (dup x) 
        (lambda (y) 
          (seq-bind (dup y) 
            (lambda (z) (progn (seq-return z))))))))

Each expression is chained into the monad such that the results of the
previous expressions are monadically bound to the variables in the do
form.  A do form is a lot like the `let*` form in lisp.  In fact,
`let*` is just `domonad identity-monad`.  This isn't a coincidence.
You can implement `let*` as a nest of function applications.

    (let* ((x 1)
           (y (+ x 1)))
        (+ x y))

Is the same as 

    (funcall (lambda (x) 
           (funcall (lambda (y)
                      (+ x y)) (+ x 1))) 1)

A monad is just a controlled way of extending the notion of function
composition and a `do` form is just a `let*` form with those modified
function composition semantics.

We can do really neat things with monads.  The sequence monad lets you
build up lists a lot like a list comprehension in a language like
python.  Want the list of all possible combinations of three sets of
symbols?


    (domonad monad-seq 
      [s1 '(a b c)
       s2 '(d e f)
       s3 '(g h i)]
     (list s1 s2 s2)) -> ((a d d) (a d d) (a d d) (a e e) (a e e) (a e
      e) (a f f) (a f f) (a f f) (b d d) (b d d) (b d d) (b e e) (b e
      e) (b e e) (b f f) (b f f) (b f f) (c d d) (c d d) (c d d) (c e
      e) (c e e) (c e e) (c f f) (c f f) (c f f) ...)

To the Graph Monad
------------------

I have it on good authority that one can think of many data structures
as having an accompaning monad.  Can we try to "discover" the monad
associated with other data structures?  Heedless of utility or an
efficient use of my time, I set out to do just that.  The data
structure I considered was the symmetric weighted graph.  This is a
data structure consisting of a set of nodes and a set of edges, which
connect nodes.  Furthermore, we enforce symmetry, so that the edge '(a
b) between nodes a and b is considered the same as the edge '(b a).
We are going to represent this structure using the inefficient but
easy to grasp association-list.  Here is some code to create an empty
weighted graph:


    (defun* empty-weighted-graph (&key (symmetric t) (pred #'equal))
      "Create an empty weighted-graph.  Not usually called directly."
      (alist>> :nodes nil ; the nodes
           :connections nil ; the edges
           :symmetric symmetric ; whether edges should be symmetric or go both ways.
           :pred pred))

Each entry in the alist is stored as a (key val) pair.  We'll use this
skeleton to store our alist.  We can add a node thusly:

    (defun wg-add-node (wg node)
      "Add a NODE to a weighted graph WG."
      (alist-add-to-set wg :nodes node (alist wg :pred)))

And a connection as so:

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
               (c-nodes 
                (if (alist wg :symmetric)
                    (wg-pair-to-canonical-order wg node1 node2)
                  (list node1 node2)))
               (connections (alist wg :connections)))
          (if connection
              (alist>> wg :connections
       (pred-alist-conjugate    wg :pred) 
            connections c-nodes (lambda (old) (+ old connection)) 0))
            (alist>> wg :connections (pred-dissoc (alist wg :pred)
            connections c-nodes)))))

That is a lot of static just to do something simple: If either node1
or node2 isn't in the graph (based on the predicate field of the
graph), they are added to a set stored in :nodes.  If the edge is not
in the graph, it is added.  If the edge is in the graph, it is added
to the value passed in, unless nil is passed in.  In the case of a NIL
connection, the connection is removed from the set.  The connections
are stored as an a-list of node-pair value pairs.  The node pairs are
sorted into "canonical order" before storage, to ensure graph
symmetry.  Canonical order is just the order the nodes were added to
the graph so that graph nodes for which there is no standard ordering
can be ordered in a predictable way.

We need just one more function.  If you are paying attention, bells
should go off when we discuss it:


    (defun wg-combine (wg1 wg2)
      "Combine weighted graphcs WG1 and WG2 using the symmetry
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

This function uses `wg-add-connection` to add all the connections
in `WG2` to `WG1`.  Since `wg-add-connection` adds connection
strengths together, the result is a graph reflecting the connectivity
of both graphs.

The bell that should have gone off in your head is that we've very
nearly defined our `bind` and `return` operations.  If our nodes hold
our monadic values, then monadic functions are functions which take
nodes and return a weighted graph.  One way of defining bind is then:

    (defun weighted-graph-bind (v f)
      "Bind the value V, a weighted graph, with the function F,
    which should take a node value and return a weighted graph 
    to be combined with V."
      (reduce 
       (lambda (wg node)
         (wg-combine wg (funcall f node)))
       (alist v :nodes)
       :initial-value (alist>> v 
                        :symmetry t)))


This is pretty straightfoward.  `Bind` takes a weighted graph and a
function which takes nodes and returns a weighted graph, applies `f`
to all the nodes in the input graph and then mergers them together,
starting with the input graph.  

Since this monad will let us build up graphs out of smaller graphs, we
should define some convenient functions to create small graphs.  These
will appear in the expression forms of `domonad` and give our
operations a pleasantly domain-specific-language feel.

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

    (defun edge (from to stren)
      "Create a graph with a single edge FROM to TO with strength STREN."
      (wg :connections (list from to stren)))

    (defun unedge (from to) 
      "Specify a weighted graph which 
      explicitely doesn't contain an edge FROM TO."
      (alist>> (empty-weighted-graph) :connections `(((,from ,to) nil))))
    
The only weird one here is `unedge` which builds a graph with an
explicit `nil` edge.  `Wg-combine` has a bit of logic built in which
removes an edge in the result graph when the merged graph contains an
explicitely `nil` edge.  This will let us delete as well as modify
edges in our graphs from within a monadic function.  Note finally that
`node` is the monad's `return` function.

It takes a value and wraps it in the monad.  Using the eclectic monad
library I've written for emacs lisp, we define a new monad:

    (defvar weighted-graph-monad 
      (tbl! :m-bind #'weighted-graph-bind
        :m-return #'wg-return)
        "The weighted graph monad.")

We can now invoke the `domonad` form with `weighted-graph-bind`.  

Let's do a simple example.  Lets build a graph which has the three
nodes `(a b c)` and edges between every member and every other except
when the members are identical.  

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

A more complex example follows.  In it we create a set of constructors
and predicates to create and identify `men` and `women`.  We then use
the graph monad to simulate a sexual contact network men and women,
using different probabilities of sexual encounter depending on whether
relations are homo or hetero-sexual.  It occurs to me that this is a
kind of weird example.  

    (labels 
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
                        connected-pop)))

If you `(require 'weighted-graph-monad)` you should be able to run
this and get a graph out that reflects one possible pass through a
swingers party in the 70's.
