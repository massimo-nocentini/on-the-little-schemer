(defun dot-name (exp)
  (substitute-if #\_ 
		 (complement (function alphanumericp)) 
		 (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      ;; here we use the function write-to-string because it allow to pass
      ;; the keyword parameter :pretty, while it is not possible doing this
      ;; using the function prin1-to-string.
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s
	    )
	)
      ""
      ))

(defun nodes->dot (nodes)
  ;; about this function we are interested to its side effects, so we are
  ;; interested about the stuff printed into the REPL
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

(defun graph->png (fname nodes edges)
  (dot->png fname 
	    (lambda ()
	      (graph->dot nodes edges))))