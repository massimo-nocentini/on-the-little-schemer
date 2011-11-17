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
;; a new list composed by only edges that has as car component node
;; equals of the given node identifier
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  ;; here we define the variable 'visited' to be an empty list. This
  ;; variable encapsulate the transitive closure of the reachability
  ;; relation, hence will contains all the nodes that can be reached
  ;; starting from the node 'node', eventually travelling one or more
  ;; nodes. This list is modified by recursive calls of the function
  ;; 'traverse'
  (let ((visited ()))
    ;; here we define a function, giving it the possibility of calling
    ;; itself (we use the function 'labels'
    (labels ((traverse (node)
	       ;; the following check is necessary to stop the
	       ;; recursion, otherwise we visit each node infinitely
	       ;; many times because our reachability relation is
	       ;; symmetric (see the implementation of the function
	       ;; 'edge-pair'). For instance if exists the edge (3
	       ;; . 4), when we call this function giving 3, the
	       ;; function will be invoked eventually giving 4, which,
	       ;; by symmetry, have an edge (4 . 3) which will cause
	       ;; the invocation and so on...
	       (unless (member node visited)
		 ;; here we add the node to the visited list
		 (push node visited)
		 (mapc (lambda (edge)
			 ;; with this recursive invocation we apply
			 ;; the same rules for adding to the visited
			 ;; list all the neighbors.
			 (traverse (cdr edge)))
		       ;; here we apply the above lambda function to
		       ;; the subset of edges that have as car
		       ;; component the given node, hence the set of
		       ;; nodes that we can reach from the node
		       ;; 'node'. In this way we can scan the list of
		       ;; neighbors of node 'node'
		       (direct-edges node edge-list)))))
      (traverse node))
    ;; here we return the list containing the transitive closure of
    ;; the reachability relation starting from node 'node'
    visited))

(defun find-islands (nodes edge-list)
  ;; we define a list to be empty, which will be filled during
  ;; recursive invocations
  (let ((islands ()))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))