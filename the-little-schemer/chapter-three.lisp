(in-package :com.github.massimo-nocentini.the-little-schemer)

(defun rember (a lat)
  (cond
    ((null lat) (quote ()))
    ((eq a (car lat)) (cdr lat))
    (t (cons			;here we know that (car lat) is not
				;equal to the element to remove, so we
				;have to save it building a cons
				;structure
	     (car lat)
	     (rember a (cdr lat))))))

(defun firsts (l)
  (cond
    ((null l) (quote ()))
    (t (cons (car (car l))		;typical element
	     (firsts (cdr l))))))	;natural recursion

(defun insertR (new old lat)
  (cond ((null lat) (quote ()))
	((eq old (car lat)) (cons old	;we could use (car lat) also,
					;but this implies a
					;computation, old is given
					;and, in this case, is equals
					;to (car lat)
				  (cons new
					(cdr lat))))
	(t (cons (car lat)
		 (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  "Contract: atom atom list-of-atom -> list-of-atom

This function, given an element OLD to search, produce a new list such
that every occurrence of OLD is preceded by an occurrence of the atom
NEW."
  (cond ((null lat) (quote ()))
	((eq old (car lat)) (cons new lat )) ;here we can use the
					     ;entire lat as base for
					     ;consing because (eq old
					     ;(car lat)) is t
	(t (cons (car lat)
		 (insertL new old (cdr lat))))))

(defun my-subst (new old lat)
    "Contract: atom atom list-of-atom -> list-of-atom

This function, given an atom OLD to research in a given list LAT,
return a new list such that the first occurrence of atom OLD is
substituted by the atom NEW."
  (cond ((null lat) (quote ()))
	((eq old (car lat)) (cons new (cdr lat)))
	(t (cons (car lat)
		 (my-subst new old (cdr lat))))))


(defun my-subst2 (new o1 o2 lat)
  (cond ((null lat) (quote ()))
	((or (eq o1 (car lat))
	     (eq o2 (car lat))) (cons new (cdr lat)))
	(t (cons (car lat)
		 (my-subst2 new o1 o2 (cdr lat))))))


(defun multirember (a lat)
    "Contract: atom list-of-atom -> list-of-atom

This function, given an atom A and a list LAT, return a new list that
doesn't contain any atom A."
  (cond
    ((null lat) (quote ()))
    ((equal-sexps a (car lat))
     (multirember a (cdr lat))) ;we recur on the (cdr lat) because
				;what we want is the remainder of the
				;list such that doesn't contains any
				;'a
    (t (cons			;here we know that (car lat) is not
				;equal to the element to remove, so we
				;have to save it building a cons
				;structure
	     (car lat)
	     (multirember a (cdr lat))))))

(defun multi-insert-r (new old lat)
  "Contract: atom atom list-of-atom -> list-of-atom

This function, given an atom OLD to research in a given list LAT,
return a new list that add the atom NEW to the right of every
occurrences of atom OLD."
  (cond ((null lat) (quote ()))		;always the First Commandment
	((eq old (car lat)) (cons old	;we could use (car lat) also,
					;but this implies a
					;computation, instead old is
					;given and, in this branch, is
					;equals to (car lat)
				  (cons new
					(multi-insert-r new old ;inductive
								;step
							(cdr lat)))))
	(t (cons (car lat)
		 (multi-insert-r new old (cdr lat))))))

(defun multi-insert-l (new old lat)
  "Contract: atom atom list-of-atom -> list-of-atom

This function, given an atom OLD to research in a given list LAT,
return a new list that add the atom NEW to the left of every
occurrences of atom OLD."
  (cond ((null lat) (quote ()))
	((eq old (car lat)) (cons new
				  (cons old
					(multi-insert-l new
							old
							(cdr lat)))))
	(t (cons (car lat)
		 (multi-insert-l new old (cdr lat))))))

(defun multi-subst (new old lat)
    "Contract: atom atom list-of-atom -> list-of-atom

This function, given an atom OLD to research in a given list LAT,
return a new list such that every occurrences of atom OLD is
substituted by the atom NEW."
  (cond ((null lat) (quote ()))
	((eq old (car lat)) (cons new
				  (multi-subst new
					       old
					       (cdr lat))))
	(t (cons (car lat)
		 (multi-subst new old (cdr lat))))))