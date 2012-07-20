(in-package :com.github.massimo-nocentini.the-little-schemer)

(defun latp (lst)
  "This function return true if the argument LST is a list that
doesn't contain any lists, nil otherwise."
  (cond
    ((null lst) t)
    ((atomp (car lst)) (latp (cdr lst)))
    (t nil) ) )

(defun nonlatp (lst)
  "This function return true if LST is a non empty list and contains
  no atomic sexp"
  (cond
    ((or (null lst) (atomp (car lst))) nil)
    (t t)) )

(defun memberp-lists-inside-guarded (a lat)
  "This function return true if the argument A is a member of the list LAT"
  (cond
    ((null lat) nil)
    ((atomp (car lat)) (or (eq a (car lat))
			   (memberp a (cdr lat))))
    (t (memberp a (cdr lat)))))

(defun memberp (a lat)
  "This function return true if the argument A is a member of the list LAT"
  (cond
    ((null lat) nil)
    (t (or (equal-sexps a (car lat)) (memberp a (cdr lat))))))

(defun member2p (a lat)
  (cond
    ((null lat) nil)
    (t (or (member2p a (cdr lat)) (eq a (car lat))))))

(defun member-twice (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat)) (memberp a (cdr lat)))
    (t (member-twice a (cdr lat)))))
