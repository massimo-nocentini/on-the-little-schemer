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
    ((null lat) (reverse acc))		;just reverse the accumulated
    ((null (car lat))
     (multiup-with-acc (cdr lat) acc))	;ignore the empty list in (car
					;lat) and recur
    ((onep (length (car lat)))
     (multiup-with-acc (cdr lat)
		       (cons (car (car lat)) acc))) ;'up-ing'
    (t (multiup-with-acc (cdr lat) (cons (car lat) acc)))))

(defun down*-with-acc-prefix (l acc)
  "Given a list and an accumulator, returns a list such that contains
each atom in l, downed of one level. This function visit the list in
prefix order."
  (cond
    ((null l) (reverse acc))	
    ((atomp (car l)) (down*-with-acc-prefix
		      (cdr l)		       ;recur and ...
		      (cons (cons (car l) '()) ;push down
			    acc)))
    (t (down*-with-acc-prefix		;*Oh My Gawd*
	(cdr l)
	(cons (down*-with-acc-prefix (car l) '())
	      acc)))))

;; WRONG!! I'm not able to write this postfix version...It can be
;; written preserving tail recursion?
(defun down*-with-acc-postfix (l acc)
  "Given a list and an accumulator, returns a list such that contains
each atom in l, downed of one level. This function visit the list in
postfix order."
  (cond
    ((null l) (reverse acc))	
    ((atomp (car l)) (down*-with-acc-postfix
		      (cdr l)		       ;recur and ...
		      (cons (cons (car l) '()) ;push down
			    acc)))
    (t (cons (down*-with-acc-postfix
	(car l)				;*Oh My Gawd*
	'()) 
	(down*-with-acc-postfix (cdr l) acc)) )))

(defun down* (l)
  (cond
    ((null l) '())
    ((atomp (car l)) (cons (cons (car l) '()) (down* (cdr l))))
    (t (cons (down* (car l)) (down* (cdr l))))))

(defun up* (l)
  (cond
    ((null l) '())			;() to allow list construction
    ((atomp (car l))			;just keep the atom
     (cons (car l) (up* (cdr l))))
    ((null (car l)) (up* (cdr l)))	;ignore the empty list
    ((onep (length (car l)))		;keep the singleton atom
     (cons (car (car l)) (up* (cdr l)))) 
    (t (cons (up* (car l)) (up* (cdr l))))) ) ;*Oh My Gawd*

(defun up*-with-acc-prefix (l acc)
  (cond
    ((null l) (reverse acc))
    ((atomp (car l)) (up*-with-acc-prefix (cdr l)
					  (cons (car l) acc)))
    ((null (car l)) (up*-with-acc-prefix (cdr l) acc))
    ((onep (length (car l)))
     (up*-with-acc-prefix (cdr l)
			  (cons (car (car l))
				acc)))
    (t (up*-with-acc-prefix (cdr l)
			    (cons (up*-with-acc-prefix (car l) '())
				  acc)))))

;; just to do a proposed exercise, we implement the occur* function
;; using as always the accumulation technique
(defun occur*-with-acc (a l acc)
  (cond
    ((null l) acc)
    ((atomp (car l))
     (cond
       ((eq a (car l)) (occur*-with-acc a (cdr l) (1+ acc)))
       (t (occur*-with-acc a (cdr l) acc))))
    ;;here we proceed in postfix. Notice that here the order in which
    ;;we recur (ie. first on cdr or first on the car) doesn't matter
    ;;because the SUM function is commutative...too much simpler than
    ;;the down*-with... and up*-with... functions.
    (t (occur*-with-acc a (cdr l) 	
			(occur*-with-acc a (car l) acc)))))

