(in-package :com.github.massimo-nocentini.the-little-schemer)

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

(defun pair-third-component (pair)
  "contract: pair -> sexp"
  (car (cdr (cdr pair))) )

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

;; *************************************************************
;; EXERCISES
;; *************************************************************

(defun domset (rel acc)
  "This function computes the domain of discurse of the relation
  rel. We use the accumulator technique in order to make a single call
  to MAKESET in the base of recursion."
  (cond
    ((null rel) (makeset acc))
    (t (domset (cdr rel) (cons (pair-first-component (car rel))
			       (cons (pair-first-component (car rel))
				     acc))))))

(defun idrel (aSet)
  (cond
    ((null aSet) '())
    (t (cons (build-pair (car aSet) (car aSet))
	     (idrel (cdr aSet))))))

(defun reflexive? (aRel)
  (tls-subsetp (idrel (domset aRel '())) aRel))
  ;; (reflexive?-helper aRel (domset aRel '())))

(defun reflexive?-helper (aRel domsetOfaRel)
  (cond
    ((null domsetOfaRel) t)
    (t (and (memberp (build-pair (car domsetOfaRel) ;here we're doing
						    ;the same stuff
						    ;that IDREL does!
				 (car domsetOfaRel))
		     aRel)
	    (reflexive?-helper aRel (cdr domsetOfaRel))))))

(defun symmetric? (aRel)
  (eqsetp aRel (revrel aRel)))

(defun anti-symmetric? (aRel)
  (tls-subsetp (intersect aRel (revrel aRel)) ;if in aRel exists two
					      ;symmetric pairs than
					      ;INTERSECT contains both
					      ;of them, hence it isn't
					      ;anti-symmetric
	       (idrel (domset aRel '()))))

(defun asymmetric? (aRel)
  (null (intersect aRel (revrel aRel))))

(defun apply-function (aFun arg)
  (cond
    ((null aFun) nil)			;the function aFun is
					;undefined in ARG
    ((eq arg (pair-first-component (car aFun)))
     (pair-second-component (car aFun)))
    (t (apply-function (cdr aFun) arg))))

(defun compose-functions (composing-fun composed-fun)
  (cond
    ((null composed-fun) '())
    ;; use the LET block only for giving labels, not for state
    (t (let ((first-component (pair-first-component 
			       (car composed-fun)))
	     (second-component (pair-second-component
				(car composed-fun))))
	 (cond
	   ((memberp second-component (firsts composing-fun))
	    (cons (build-pair first-component
			      (apply-function composing-fun
					      second-component))
		  (compose-functions composing-fun
				     (cdr composed-fun))))
	   (t (compose-functions
	       composing-fun (cdr composed-fun))))))))

(defun apply-relation (aRel arg acc)
  (cond
    ((null aRel) (makeset acc))
    ((eq arg (pair-first-component (car aRel)))     
     (apply-relation (cdr aRel)
		     arg
		     (cons (pair-second-component (car aRel))
			   acc)))
    (t (apply-relation (cdr aRel) arg acc))))

(defun rel-in (arg aSet)
  (cond
    ((null aSet) '())
    (t (cons (build-pair arg (car aSet))
	     (rel-in arg (cdr aSet))))))

(defun compose-relations (rel1 rel2)
  (cond
    ((null rel1) '())
    (t (tls-union
	(rel-in (pair-first-component (car rel1))
		(apply-relation rel2
				(pair-second-component
				 (car rel1))
				'()))
	(compose-relations (cdr rel1) rel2)))))

(defun transitive? (aRel)
  (tls-subsetp (compose-relations aRel aRel) aRel))

(defun quasi-order? (aRel)
  (and (reflexive? aRel) (transitive? aRel)))

(defun partial-order? (aRel)
  (and (quasi-order? aRel) (anti-symmetric? aRel)))

(defun equivalence-rel? (aRel)
  (and (quasi-order? aRel) (symmetric? aRel)))

(define-test friends-and-relations

  (assert-true (eqsetp '(a b) (domset '((a b) (a a) (b b)) '())))
  (assert-equal '(c) (domset '((c c)) '()))
  (assert-true (eqsetp '(a b) (domset '((a c) (b c)) '())))

  (assert-true (eqsetp '((a a) (b b)) (idrel '(a b))))
  (assert-true (eqsetp '((c c) (d d)) (idrel '(c d))))
  (assert-true (eqsetp '() (idrel '())))

  (assert-true (reflexive? '((a b) (a a) (b b))))
  (assert-true (reflexive? '((c c))))
  (assert-false (reflexive? '((a c) (b c))))

  (assert-false (symmetric? '((a b) (a a) (b b))))
  (assert-true (symmetric? '((c c))))
  (assert-true (symmetric? '()))
  
  (assert-true (anti-symmetric? '((a b) (a a) (b b))))
  (assert-true (anti-symmetric? '((c c))))
  (assert-false (anti-symmetric? '((a b) (b a))))
  
  (assert-false (asymmetric? '((a b) (a a) (b b))))
  (assert-true (asymmetric? '()))
  (assert-true (asymmetric? '((a c) (b c))))

  (assert-equal 1 (apply-function '((a 1) (b 2) (c 2) (d 9)) 'a))
  (assert-equal nil (apply-function '() 'a))
  (assert-equal 2 (apply-function '((a 2) (b 1)) 'a))

  (assert-equal '() (compose-functions
		     '((a 1) (b 2) (c 2) (d 9))
		     '((1 $) (3 *))))
  (assert-equal '() (compose-functions
		     '((a 1) (b 2) (c 2) (d 9))
		     '((a 2) (b 1))))
  (assert-equal '((a $) (d $)) (compose-functions
				'((1 $) (3 *))
				'((a 1) (b 2) (c 2) (d 1))))
  (assert-equal '((b $)) (compose-functions
				'((1 $) (3 *))
				'((a 2) (b 1))))

  (assert-true (eqsetp '(1 2) (apply-relation
		      '((a 1) (b 2) (a 2) (d 1) (a 1)) 'a '())))
  (assert-true (eqsetp '(a b) (apply-relation
			       '((a b) (a a) (b b)) 'a '())))
  (assert-equal '() (apply-relation '() 'a '()))

  (assert-equal '((a a) (a b)) (rel-in 'a '(a b)))
  (assert-equal '((a c) (a d)) (rel-in 'a '(c d)))
  (assert-equal '() (rel-in 'a '()))

  (assert-true (eqsetp '((a c) (b c))
		       (compose-relations
			'((a a) (a b) (b b))
			'((a c) (b c)))))
  (assert-true (eqsetp '((a 1) (a 2) (b 2))
		       (compose-relations
			'((a a) (a b) (b b))
			'((a 1) (b 2) (c 2) (d 1)))))
  (assert-true (eqsetp '((a a) (a b) (a b) (b b))
		       (compose-relations
			'((a a) (a b) (b b))
			'((a a) (a b) (b b)))))

  (assert-true (transitive? '((a a) (a b) (b b))))
  (assert-true (transitive?  '((a c) (b c))))
  (assert-true (transitive? '((a 1) (b 2) (c 2) (d 1))))
  


  
  )