(defun atomp (x)
  "This function return true if the argument X is an atom, nil
otherwise."
  (not (listp x)))

(defun latp (lst)
  "This function return true if the argument LST is a list that
doesn't contain any lists, nil otherwise."
  (cond
    ((null lst) t)
    ((atomp (car lst)) (latp (cdr lst)))
    (t nil) ) )

(defun memberp (a lat)
  "This function return true if the argument A is a member of the list LAT"
  (cond
    ((null lat) nil)
    (t (or (eq a
	       (car lat))
	   (memberp a (cdr lat))))))


