(in-package :com.github.massimo-nocentini.the-little-schemer)

(defun rember2 (a lat)
  (cond
    ((null lat) ())
    ((eq a (car lat)) (cons a (rember a (cdr lat))))
    (t (cons (car lat) (rember2 a (cdr lat))))))


(defun seconds (l)
  (cond
    ((null l) (quote ()))
    (t (cons (car (cdr (car l)))        ;typical element
	     (seconds (cdr l))))))	;natural recursion

(defun dupla (a lat)
  (cond
    ((null lat) ())
    (t (cons a (dupla a (cdr lat))))))

(defun double (a lat)
  (cond
    ((null lat) ())
    ((eq a (car lat)) (cons a lat))	;since (eq a (car lat)) holds,
					;consing a new 'a' onto lat we
					;double the 'a' symbol
    (t (cons (car lat) (double a (cdr lat)))) ))


(defun subst-sauce (a lat)
  (my-subst a 'sauce lat))


(defun subst3 (new o1 o2 o3 lat)
  (cond ((null lat) (quote ()))
	((or (eq o1 (car lat))
	     (eq o2 (car lat))
	     (eq o3 (car lat))) (cons new (cdr lat)))
	(t (cons (car lat)
		 (subst3 new o1 o2 o3 (cdr lat))))))

(defun subst-other-list-occurring (new checking-lat lat)
  (cond
    ((null lat) ())
    ((memberp (car lat) checking-lat) (cons new (cdr lat)))
    (t (cons (car lat)
	     (subst-other-list-occurring new
					 checking-lat
					 (cdr lat))))))

(defun duplicate (n obj)
  (cond
    ((zerop n) '())
    (t (cons obj (duplicate (1- n) obj)))) )

(defun index (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat)) (1+ 0))
    (t (1+ (index a (cdr lat))))))

(defun index-with-acc (a lat acc)
  (cond
    ((null lat) nil)			;instead to return the
					;accumulator, here we return
					;nil to indicate that the
					;value isn't a member of lat
    ((eq a (car lat)) (1+ acc))
    (t (index-with-acc a (cdr lat) (1+ acc)))))

(defun dot-product-with-acc (tup1 tup2 acc)
  (cond
    ((null tup1) acc)
    (t (dot-product-with-acc (cdr tup1)
			     (cdr tup2)
			     (o+ (x (car tup1) (car tup2)) acc)))))

(defun multidown-with-acc (lat acc)
  (cond
    ((null lat) (reverse acc))		;we have to reverse the
					;accumulated list because we
					;cannot build it with the atom
					;in the final right order
					;because this function is
					;called with '() as initial
					;ACC, hence if in the
					;following line we cons ACC
					;before the 'downed' atom, we
					;obtain an empty list in the
					;first position of the final
					;ACC
    (t (multidown-with-acc (cdr lat) (cons (cons (car lat) ()) acc)))))

(defun multiup-with-acc (lat acc)
  (cond
    ((null lat) (reverse acc))
    ((null (car lat)) (multiup-with-acc (cdr lat) acc))
    ((onep (length (car lat)))
     (multiup-with-acc (cdr lat)
		       (cons (car (car lat)) acc)))
    (t (multiup-with-acc (cdr lat) (cons (car lat) acc)))))
