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
  (cond ((null l) (quote ()))
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
    ((eq a (car lat)) (multirember a (cdr lat))) ;we recur on the (cdr
						 ;lat) because what we
						 ;want is the
						 ;remainder of the
						 ;list such that
						 ;doesn't contains any
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

(defun o+ (n m)
  "Contract: number number -> number"
  (cond ((zerop m) n)
	(t (o+ (1+ n) (1- m)))) )

;; a subtle observation about this implementation and the one written
;; in the book: this version is tail-recursive, that is no work have
;; to be done when we reach the base of the recursion. In my head
;; there are two stack, one with n objects, the other with m
;; objects. Every recursive call pop an object from the second stack
;; and push it on the first one. When the second stack is empty the
;; recursion end and the result is already built in the first
;; stack. In the book version there is something more elengant in my
;; opinion: what is done is make a correspondence between the
;; recursive call with the knowledge of how many time we have to apply
;; the 1- function on the first argument. In other work we catch in
;; the computation (the recursive calls) a knowledge about data, so we
;; can forget the value of m during the work after the base of
;; recursion is reached. However that implementation is not tail
;; recursive.
(defun o- (n m)
  "Contract: number number -> number"
  (cond ((zerop m) n)
	(t (o- (1- n) (1- m)))) )

(defun addtup (tup)
  "Contract: tup-of-numbers -> number"
  (cond ((null tup) 0)
 	(t (o+ (car tup) (addtup (cdr tup))))))

(defun x (n m)
  "Contract: number number -> number"
  (cond ((zerop m) 0)
	(t (o+ n (x n (1- m))))))

(defun tup+ (tup1 tup2)
  "Contract: list-of-numbers list-of-numbers -> list-of-numbers"
  (cond ((null tup1) tup2)
	((null tup2) tup1)
	(t (cons (o+ (car tup1)
		     (car tup2))
		 (tup+ (cdr tup1)
		       (cdr tup2))))))

(defun greater-than (n m)
  "Contract: number number -> boolean

Observation: this function require that the two parameters N and M are
non negative integers"
  (cond ((zerop n) nil)			;if we reason only about whole
					;integers than 0 is less than
					;or equals to any other
					;integer, so in all two cases
					;the answer is false. Observe
					;that if we swap the two
					;questions, the function is
					;not correct.
	((zerop m) t)			;if we ask this question, N
					;must be some positive
					;integer, hence it is always
					;greater than M which, in this
					;question is 0, so we answer
					;true.
	(t (greater-than (1- n) (1- m))))) ;otherwise we natural recur
					   ;on both numbers

(defun less-than (n m)
  "Contract: number number -> boolean
Observation: this function require that the two parameters N and M are
non negative integers"
  (cond ((zerop m) nil)	      		;if m is zero than any other
					;number n is at least equals
					;or greater than, in both
					;cases not less. Hence we
					;answer false
	((zerop n) t)			;if we reason only about whole
					;integers than 0 is less than
					;or equals to any other
					;integer, and if we ask this
					;question then m is surely a
					;positive integer, hence we
					;answer t. Observe that if we
					;swap the two questions, the
					;function is not correct.
	(t (less-than (1- n) (1- m))))) ;otherwise we natural recur on
					;both numbers

(defun our-equal (n m)
  (cond
    ((zerop m) (zerop n))
    ((zerop n) nil)			;here we know that M isn't
					;zero, so if N is zero surely
					;they are different
    (t (our-equal (1- n) (1- m)))	;use natural recursion to find
					;out the answer
    )
  )


(defun our-expt (base exponent)
  "Contract: number number -> number"
  (cond
    ((zerop exponent) 1)		;by the fifth commandment we
					;return 1 as termination value
					;because we're building a
					;number with 'x'
    (t (x base (expt base (1- exponent)))) ) )

(defun integer-division (dividend divisor)
  "Contract: number number -> number

How many times divisor is in dividend space?"
  (cond
    ((less-than dividend divisor) 0) 		;by the fifth
						;commandment relative
						;to 'addition'
    (t (1+ (integer-division (o- dividend divisor) divisor))) ))

(defun our-length (lat)
  "Contract: list-of-atom -> number"
  (cond
    ((null lat) 0)
    (t (1+ (our-length (cdr lat))))	;here we know that at least an
					;atom is present in the list
					;LAT, so remember that with 1+
    ) )

(defun our-pick (n lat)
  "Contract: number list-of-atom -> atom"
  (cond
    ((zerop (1- n)) (car lat) )
    (t (our-pick (1- n) (cdr lat)))) )

(defun rempick (n lat)
  "Contract: number list-of-atom -> list-of-atom"
  (cond
    ((zerop (1- n)) (cdr lat))		;we have to discard the car
					;element because we've
					;decremented N to the
					;requested index
    (t (cons (car lat)
	     (rempick (1- n) (cdr lat)))) ;use the natural recursion
					  ;onto both N and LAT
    ) )

(defun no-nums (lat)
  "Contract: list-of-atom -> list-of-atom"
  (cond
    ((null lat) (quote ()))
    (t (cond
	 ((numberp (car lat)) (no-nums (cdr lat)) )
	 (t (cons (car lat) (no-nums (cdr lat)))))) ) )

(defun all-nums (lat)
  "Contract: list-of-atom -> tuple"
  (cond
    ((null lat) (quote ()))
    (t (cond
	 ((numberp (car lat)) (cons (car lat)
				    (all-nums (cdr lat))))
	 (t (all-nums (cdr lat)))))) )

(defun eqan (a1 a2)
  "Contract: atom atom -> boolean"
  (cond
    ((and (numberp a1) (numberp a2)) (our-equal a1 a2))
    ((or (numberp a1) (numberp a2)) nil) ;if at least one of them is a
					 ;number, so they are not the
					 ;same atom
    (t (eq a1 a2))) )

(defun occur (a lat)
  "Contract: atom list-of-atom -> number"
  (cond
    ((null lat) 0) 			;by first commandment follow
					;the condition on the argument
					;that change during
					;recursion. By the Fifth
					;commandment we return 0
					;because we're building a
					;number with the operator +
    (t (cond
	 ((eqan a (car lat)) (1+ (occur a (cdr lat))))
	 (t (occur a (cdr lat))))) ) )

(defun onep (n)
  "Contract: number -> boolean"
  (zerop (1- n)) )

(defun rempick-using-one (n lat)
  "Contract: number list-of-atom -> list-of-atom"
  (cond
    ((onep n) (cdr lat))
    (t (cons (car lat) (rempick-using-one (1- n) (cdr lat))))))

(defun rember* (a l)
  "Contract: atom list-of-sexp -> list-of-sexp"
  (cond
    ((null l) (quote ()))		;if the list is empty we have
					;nothing to remove
    ((atomp (car l))			;if the car of L is an atom we
					;are able to perform a check
					;with the given A to remove
     (cond
       ((eqan a (car l)) (rember* a (cdr l))) ;if the car is really A
					      ;we return what return
					      ;this function applied
					      ;to the cdr of L
					      ;(assuming by induction
					      ;that this function is
					      ;correct for cdr of
					      ;lists, this step is
					      ;what really remove the
					      ;atom a
       (t (cons (car l) (rember* a (cdr l)))))) ;otherwise we keep the
						;atom A, consing it on
						;the list without atom
						;A by induction hp
    (t (cons (rember* a (car l))		;if the car of L isn't
						;an atom it must be a
						;list, so we rebuilt
						;the cons structure
						;(because a list is at
						;the end a cons
						;structure) with the
						;two recursive
						;applications of these
						;rules (another way to
						;call a (this)
						;function)
	     (rember* a (cdr l)))))
  )