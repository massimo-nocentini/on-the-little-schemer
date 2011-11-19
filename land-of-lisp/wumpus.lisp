(load "dot-file-generator.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply (function append) 
	 (loop repeat *edge-num*
	    collect (edge-pair (random-node) (random-node)))))

;; this function, given a node identifier and a list of edges, returns
;; a new list composed by only edges that has as car component a node
;; equals of the given node identifier. Maybe a better name for the
;; function should had been neighborhood-of-node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  ;; here we define the variable 'visited' to be an empty list. This
  ;; variable encapsulate the transitive closure of the reachability
  ;; relation, hence will contains all the nodes that can be reached
  ;; starting from the node 'node', eventually travelling zero (so the
  ;; returned list certainly contains 'node') or more nodes. This list
  ;; is modified by recursive calls of the local function 'traverse'
  (let ((visited ()))
    ;; here we define a function, giving it the possibility of calling
    ;; itself (we use the function 'labels' to build the function)
    (labels ((traverse (node)
	       ;; the following check is necessary to stop the
	       ;; recursion, otherwise we visit each node infinitely
	       ;; many times because our reachability relation is
	       ;; symmetric (see the implementation of the function
	       ;; 'edge-pair'). For instance if exists the edge (3
	       ;; . 4), when we call this function giving 3, the
	       ;; function will be invoked eventually with argument 4,
	       ;; which, by symmetry, have an edge (4 . 3) which will
	       ;; cause the invocation of this function with argument
	       ;; 3, and so on...
	       (unless (member node visited)
		 ;; here we add the node to the visited list
		 (push node visited)
		 (mapc (lambda (edge)
			 ;; with this recursive invocation we apply
			 ;; the same rules for adding to the visited
			 ;; list all the neighbors (located in the cdr
			 ;; component of the edge)
			 (traverse (cdr edge)))
		       ;; here we apply the above lambda function to
		       ;; the subset of edges that have as car
		       ;; component the given node, hence the set of
		       ;; nodes that we can reach from the node
		       ;; 'node'. In this way we can scan the list of
		       ;; neighbors of node 'node'
		       (direct-edges node edge-list)))))
      ;; here we don't care about the return value of the function, we
      ;; are only interested in the 'side-effect' of this function on
      ;; the list 'visited' defined at the top of this method. This is
      ;; a pure functional programming approach? This means that
      ;; lambda function or local function can manipulate object not
      ;; directly declared as its parameters or local variable?
      (traverse node))
    ;; here we return the list containing the transitive closure of
    ;; the reachability relation starting from node 'node'
    visited))

(defun find-islands (nodes edge-list)
  ;; we define a list to be empty, which will be filled during
  ;; recursive invocations
  (let ((islands ()))
    (labels ((find-island (nodes)
	       ;; the variable 'connected' contains a list of nodes
	       ;; reachable from the first node in the node list
	       ;; 'nodes'.
	       (let* ((connected (get-connected (car nodes) edge-list))
		      ;; we compute the nodes not reached by the first
		      ;; node in the list 'nodes'
		      (unconnected (set-difference nodes connected)))
		 ;; surely the 'connected' list is an island (and, in
		 ;; the perfect case, is a list that collect all
		 ;; nodes, hence it should be equal to 'nodes'
		 (push connected islands)
		 ;; if remains at least one node not reached by the
		 ;; first node in the list 'nodes', then we recur on
		 ;; the first non-reached node.
		 (when unconnected
		   (find-island unconnected)))))
      ;; start the effective computation
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  ;; if there exists at least two islands...  Observe that when in the
  ;; argument exists only one island, the 'when' function evaluate the
  ;; condition as false, hence return 'nil' (or () for better
  ;; comprehension of the following statement), and this 'nil' is used
  ;; as base list for applying correctly the 'append' function (which
  ;; require its last argument to be a list)
  (when (cdr islands)
    ;; here we build two symmetric edges, pairing the first node of
    ;; the first island and the first node of the second island
    (append (edge-pair (caar islands) (caadr islands))
	    ;; here we recur to connect possibly other islands, giving
	    ;; as parameter a new version of the 'islands' list,
	    ;; without its first island, already managed
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  ;; this function compose the previous ones, and return a new version
  ;; of list of edges, including all the edge necessary to bridge
  ;; islands, in order to have a congestion city completly connected.
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    ;; here we build a list consisting of the
			    ;; second component of the edge, because
			    ;; edge is a pure (dotted) cons and not a
			    ;; list)
			    (list (cdr edge)))
			  ;; the following expression first retrieve
			  ;; the neighborhood of the node 'node1'
			  ;; accessing the passed list of edges
			  ;; 'edge-list', after remove all the
			  ;; duplicated pairs using the 'equal'
			  ;; function. We have to use the function
			  ;; 'equal' exactly because the default
			  ;; function is 'eql' instead, and, known we
			  ;; use the function 'direct-edges', whom
			  ;; make a new list containing different
			  ;; cons, which can have the same car and cdr
			  ;; component but are conmpletly distinct
			  ;; cons object, with 'eql' function they are
			  ;; always different, while using the 'equal'
			  ;; function we compare the car and the cdr
			  ;; of conses, which return true for conses
			  ;; of this form: (A . B) (A . B), for some
			  ;; symbols A and B. this way we remove
			  ;; edges that have the same components, aka
			  ;; two or more edges that pair two nodes,
			  ;; returning only one of them to mantain the
			  ;; reachability relation.
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test (function equal)))))
	  ;; the following expression first retrieve all first
	  ;; components from the edge-list list, after removes all the
	  ;; duplicated nodes
	  (remove-duplicates (mapcar (function car) edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    ;; the first component is the source of the edge
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      ;; here we build conceptually the same things as before,
	      ;; a list with car component the source of the edge and
	      ;; as cdr component the list of neighbors. The only
	      ;; difference is in the addition of information on the
	      ;; edges as done below.
	      (cons node1
		    (mapcar (lambda (edge)
			      ;; every element passed in in this
			      ;; lambda function is a list, hence the
			      ;; actual neighbor is in the car
			      ;; position.
			      (let ((node2 (car edge)))
				;; here we build a list with the two
				;; nodes that we are analyzing
				;; (neighbors of each other) and check
				;; if at least one of the two pair is
				;; contained in the edges-with-cops
				;; list passed in as argument.
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  ;; here we use the
						  ;; equal function
						  ;; because we want
						  ;; to compare the
						  ;; pair of neighbors
						  ;; component by
						  ;; component.
						  :test (function equal))
				    ;; if there exists at least one
				    ;; pair in common we augment the
				    ;; reachability information adding
				    ;; the symbol 'cops' to it
				    (list node2 'cops)
				    ;; otherwise we leave the previous
				    ;; information as it was before.
				    edge)))
			    node1-edges))))
	  edge-alist))

(defun make-city-edges ()
  ;; we enumerate all possible nodes. Note the let*, which allow from
  ;; the second variable binding to refer to a variable defined before
  ;; the current one.
  (let* ((nodes (loop for i from 1 to *node-num*
		     collect i))
	 ;; getting a list of edges that allow the Congestion City to
	 ;; be fully connected.
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 ;; in this way we select randomly which edge is checked by
	 ;; the cops. It is something like an indicator function the
	 ;; lambda that we are going to use.
	 (cops (remove-if-not (lambda (x)
				;; don't use the input, the predicate
				;; is true if and only if the random
				;; invocation return zero. The
				;; probability that zero is the
				;; outcome is 1/*cop-odds* (an uniform
				;; distribution).
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))