(defun dot-name (exp)
  (substitute-if #\_ 
		 (complement (function alphanumericp)) 
		 (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      ;; here we use the function write-to-string because it allow to
      ;; pass the keyword parameter :pretty, while it is not possible
      ;; doing this using the function prin1-to-string.
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s
	    )
	)
      ""
      ))

(defun nodes->dot (nodes)
  ;; about this function we are interested to its side effects, so we
  ;; are interested about the stuff printed into the REPL.  This
  ;; function dosn't respect the functional style because don't return
  ;; what it compute, but print something to the standard output, so
  ;; the caller have to know that the interesting object of this
  ;; function are in the standard output and not in the returned
  ;; object (which is exactly the input argument).
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)
		)
	  )
	edges
	)
  )

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
  )

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  ;; (ext:shell (concatenate 'string "dot -Tpng -0 " fname))
  )

;; the fname argument here means "file name"
(defun graph->png (fname nodes edges)
  (dot->png fname 
	    (lambda ()
	      (graph->dot nodes edges))))

;; the following code implement the functionality to draw an
;; undirected graph.
(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst))) 
	   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))