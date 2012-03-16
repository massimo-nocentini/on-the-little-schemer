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
    (t (or (equal-sexps a
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
					;nothing to remove, and by
					;Fifth Commandment we return
					;() because we are building a
					;list with the operator cons.
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

(defun insert-r* (new old l)
  "Contract: atom atom list-of-sexp -> list-of-sexp"
  (cond
    ((null l) (quote ()))		;by the first commandment ask
					;for null when we recur
					;modifying a list
					;parameter. By the fifth
					;commandment return () because
					;we have to build lists with
					;cons
    ((atomp (car l))			;we ask if the car is an atom,
					;in this case...
     (cond
       ((eqan old (car l)) (cons old
				 (cons new
				       (insert-r* new old (cdr l)))))
       (t (cons (car l)
		(insert-r* new old (cdr l))))))
    (t (cons (insert-r* new old (car l))
	     (insert-r* new old (cdr l)))) ) )

(defun occur* (a l)
  "Contract: atom list-of-sexp -> number"
  (cond
    ((null l) 0)			;by the First Commandment we
					;check for termination. By the
					;Fifth Commandment we return 0
					;because we have to build a
					;number with the operator +
    ((atomp (car l))
     (cond
       ((eqan a (car l)) (1+ (occur* a (cdr l))))
       (t (occur* a (cdr l)))))
    (t (o+ (occur* a (car l))
	   (occur* a (cdr l)))) ))

(defun subst* (new old l)
  "Contract: atom atom list-of-sexp -> list-of-sexp"
  (cond
    ((null l) (quote ()))		;by first and fifth
					;commandment
    ((atomp (car l))
     (cond
       ((eqan old (car l)) (cons new
				 (subst* new old (cdr l))))
       (t (cons (car l)
		(subst* new old (cdr l))))))
    (t (cons (subst* new old (car l))
	     (subst* new old (cdr l))))
    )
  )

(defun insert-l* (new old l)
  "Contract: atom atom list-of-sexp -> list-of-sexp"
  (cond
    ((null l) (quote ()))		;guard by first commandment,
					;return by the fifth
					;commandment
    ((atomp (car l))
     (cond
       ((eqan old (car l)) (cons new
				 (cons old ;here we have to cons
					   ;because we have to change
					   ;at least one argument when
					   ;we recur: in our case this
					   ;argument is L
				       (insert-l* new old (cdr l)))))
       (t (cons (car l)
		(insert-l* new old (cdr l))))))
    (t (cons (insert-l* new old (car l))
	     (insert-l* new old (cdr l))))
    ))

(defun member* (a l)
  "Contract: atom list-of-sexp -> boolean"
  (cond
    ((null l) nil)
    ((atomp (car l)) (or (eqan a (car l))
			 (member* a (cdr l))) )
    (t (or (member* a (car l))
	   (member* a (cdr l))))) )

(defun leftmost (l)
  "Contract: list-of-sexp -> atom"
  (cond
    ((atom (car l)) (car l))
    (t (leftmost (car l)))) )

(defun eqlistp-first-revision (l1 l2)
  "Contract: list-of-sexp list-of-sexp -> boolean"
  (cond
    ((and (null l1) (null l2)) t)
    ((and (null l1) (atomp l2)) nil)
    ((null l1) nil) 			;the complete form of the
					;question should have been:
					;(and (null l1) (listp l2))

    ;; if we reach the following question then l1 must be a non-empty
    ;; list, but we don't know about the nature of l2
    ((and (atomp (car l1)) (null l2)) nil)

    ;; if we reach the following question then we know: if the car of
    ;; L1 is an atom then l2 must be a list with at least one element
    ;; (otherwise the above question should have been
    ;; answered). Otherwise, if the car of L1 is a list, we don't know
    ;; the nature of l2
    ((and (atomp (car l1)) (atomp (car l2)))
     (and (eqan (car l1) (car l2))
	  (eqlistp (cdr l1) (cdr l2))))
    ((atomp (car l1)) nil)		; if the car of L1 is an atom
					; then l2 must be a list with
					; its car is also a list, so
					; l1 is different from l2
    ;; at this point we know that the car of L1 is a list, hence now
    ;; we ask on the nature of l2
    ((null l2) nil)
    ((atomp (car l2)) nil)
    (t (and (eqlistp (car l1) (car l2))
	    (eqlistp (cdr l1) (cdr l2))))    
    ) )

(defun eqlistp-second-revision (l1 l2)
  "Contract: list-of-sexp list-of-sexp -> boolean"
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)

    ;; if we reach the following question then we know: if the car of
    ;; L1 is an atom then l2 must be a list with at least one element
    ;; (otherwise the above question should have been
    ;; answered). Otherwise, if the car of L1 is a list, we don't know
    ;; the nature of l2
    ((and (atomp (car l1)) (atomp (car l2)))
     (and (eqan (car l1) (car l2))
	  (eqlistp (cdr l1) (cdr l2))))

    ((or (atomp (car l1)) (atomp (car l2))) nil)
    
    (t (and (eqlistp (car l1) (car l2))
	    (eqlistp (cdr l1) (cdr l2))))    
    ) )

(defun equal-sexps (s1 s2)
  "Contract: sexp sexp -> boolean"
  (cond
    ((and (atomp s1) (atomp s2)) (eqan s1 s2))
    ((or (atomp s1) (atomp s2)) nil)
    (t (eqlistp s1 s2)) ) )

(defun eqlistp (l1 l2)
  "Contract: list-of-sexp list-of-sexp -> boolean"
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)
    (t (and (equal-sexps (car l1) (car l2))
	    (eqlistp (cdr l1) (cdr l2)))) ) )


(defun rember-sexp-version (sexp l)
  "Contract: sexp list-of-sexp -> list-of-sexp"
  (cond
    ((null l) (quote ()))
    ((equal-sexps sexp (car l)) (cdr l))
    (t (cons (car l) (rember-sexp-version sexp (cdr l))))))

(defun numberedp (aexp)
  "Contract: arithmetic-expression -> boolean"
  (cond
    ((atomp aexp) (numberp aexp))
    (t (and (numberedp (car aexp)) (numberedp (car
					       (cdr
						(cdr aexp)))))) ) )

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)) )

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))) )

(defun operator (aexp)
  (car aexp) )

;; using help function to hide representation we are able to focus on
;; the recursive definition of our concept. The help function allow in
;; a second moment to use the representation that is the more suitable
;; for the needs. We can therefore write a triple of help functions
;; for each representation that we have to deal with.
(defun tls-value (nexp)
  "Contract: numbered-expression -> number"
  (cond
    ((atomp nexp) nexp)			;we can return the nexp
					;because it is an atom and, by
					;contract, it is a number so
					;it is its value too.
    ((eq '+ (operator nexp)) (o+ (tls-value (1st-sub-exp nexp))
			    (tls-value (2nd-sub-exp nexp))))
    ((eq 'x (operator nexp)) (x (tls-value (1st-sub-exp nexp))
			   (tls-value (2nd-sub-exp nexp))))
    (t (expt (tls-value (1st-sub-exp nexp))
	     (tls-value (2nd-sub-exp nexp)))) ) )

(defun serop (n)
  "Contract: list-of-empty-lists -> boolean"
  (null n))

(defun edd1 (n)
  "contract: list-of-empty-lists -> list-of-empty-lists"
  (cons '() n))

(defun zub1 (n)
  "contract: list-of-empty-lists -> list-of-empty-lists"
  (cdr n) )

(defun os+ (n m)
  "contract: list-of-empty-lists list-of-empty-lists -> list-of-empty-lists"
  (cond
    ((serop m) n)
    (t (edd1 (os+ n (zub1 m)))) ) )

(defun setp (lat)
  "contract: list-of-atom -> boolean"
  (cond
    ((null lat) t)
    ((memberp (car lat) (cdr lat)) nil)
    (t (setp (cdr lat)))))

(defun makeset (lat)
  "contract: list-of-atom -> list-of-atom

This variation use the function MEMBERP as help function."
  (cond
    ((null lat) (quote ()))
    ((memberp (car lat) (cdr lat)) (makeset (cdr lat)) )
    (t (cons (car lat) (makeset (cdr lat)))) ) )

(defun makeset-variation (lat)
  "contract: list-of-atom -> list-of-atom

This variation use the function MULTIREMBER as help function."
  (cond
    ((null lat) (quote ()))
    (t (cons (car lat) (makeset-variation
			(multirember (car lat)
				     lat) ;here we can improve using
					  ;(cdr lat) to make one less
					  ;remove, that is the (car
					  ;lat) itself
			))) ) )

(defun tls-subsetp (set1 set2)
  "contract: set set -> boolean"
  (cond
    ((null set1) t)			;because the empty set is a
					;subset of every set
					;(included, of course, the
					;empty set itself!)
    (t (and (memberp (car set1) set2)
	    (tls-subsetp (cdr set1) set2))) ) )

(defun eqsetp (set1 set2)
  "contract: set set -> boolean"
  (and (tls-subsetp set1 set2)
       (tls-subsetp set2 set1))
  )

(defun intersectp (set1 set2)
  "contract: set set -> boolean"
  (cond
    ((null set1) nil)			;an empty set hasn't any
					;element that can share with
					;any other set
    (t (or (memberp (car set1) set2)
	   (intersectp (cdr set1) set2))) ) )

(defun intersect (set1 set2)
  "contract: set set -> set"
  (cond
    ((null set1) (quote ()))
    ((memberp (car set1) set2) (cons (car set1)
				     (intersect (cdr set1) set2)))
    (t (intersect (cdr set1) set2))) )

(defun tls-union (set1 set2)
  "contract: set set -> set"
  (cond
    ((null set1) set2)
    ((memberp (car set1) set2) (tls-union (cdr set1) set2))
    (t (cons (car set1) (tls-union (cdr set1) set2))) ) )

(defun tls-set-difference (set1 set2)
  "contract: set set -> set"
  (cond
    ((null set1) (quote ()))
    ((memberp (car set1) set2) (tls-set-difference (cdr set1) set2))
    (t (cons (car set1) (tls-set-difference (cdr set1) set2))) ) )

(defun intersect-all (l-set)
  "contract: list-of-set -> list-of-atom"
  (cond
    ((null (cdr l-set)) (car l-set))	;(car l-set) is a set by contract
    (t (intersect (car l-set) (intersect-all (cdr l-set)))) ) )

(defun pairp (sexp)
  "contract: list-of-sexp -> boolean"
  (cond
    ((atomp sexp) nil)			;this condition is necessary
					;because the recursive
					;definition of sexp allow to
					;be an atom or a list of sexp
    ((null sexp) nil)			;if this is true then sexp is
					;the empty list
    ((null (cdr sexp)) nil)		;if this is true then sexp
					;contains only one element
    ((null (cdr (cdr sexp))) t)		;if this is true then sexp is
					;really a pair
    (t nil) ))				;otherwise sexp have at least
					;three elements

(defun pair-first-component (pair)
  "contract: pair -> sexp"
  (car pair) )

(defun pair-second-component (pair)
  "contract: pair -> sexp"
  (car (cdr pair)) )

(defun build-pair (s1 s2)
  "contract: sexp sexp -> pair"
  (cons s1 (cons s2 (quote ()))))

(defun relationp (l-sexp)
  "contract: list-of-sexp -> boolean"
  (and (setp l-sexp) (all-pair-in-list-p l-sexp))
  )

(defun all-pair-in-list-p (l-sexp)
  "contract: list-of-sexp -> boolean"
  (cond
    ((null l-sexp) t)
    (t (and (pairp (car l-sexp)) (all-pair-in-list-p (cdr l-sexp)))) )
    )

(defun tls-functionp (rel)
  "contract: relation -> boolean"
  (setp (firsts rel)) )

(defun revrel (rel)
  "contract: relation -> relation"
  (cond
    ((null rel) (quote ()))
    (t (cons (revpair (car rel))
	     (revrel (cdr rel)))) ) )

(defun revpair (pair)
  "contract: pair -> pair"
  (build-pair (pair-second-component pair)
	      (pair-first-component pair)) )

(defun fullfunp (fun)
  "contract: function -> boolean"
  (tls-functionp (revrel fun)) )


  