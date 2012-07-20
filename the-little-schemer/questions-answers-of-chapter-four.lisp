(in-package :com.github.massimo-nocentini.the-little-schemer)

(lisp-unit:define-test questions-of-chapter-four
  (lisp-unit:assert-true 
     (atomp 14))

  (lisp-unit:assert-eq 68
		       (1+ 67))

  (lisp-unit:assert-eq 4
		       (1- 5))

  (lisp-unit:assert-eq -1
		       (1- 0))

  (lisp-unit:assert-true (zerop 0))

  (lisp-unit:assert-false (zerop 1492))

  (lisp-unit:assert-eq 58
		       (o+ 46 12))

  (lisp-unit:assert-eq 11
		       (o- 14 3))

  (lisp-unit:assert-eq 8
		       (o- 17 9))

  (lisp-unit:assert-eq -7
		       (o- 18 25))
  
  (lisp-unit:assert-eq 18
		       (addtup '(3 5 2 8)))

  (lisp-unit:assert-eq 43
		       (addtup '(15 6 7 12 3)))

  (lisp-unit:assert-eq 15
		       (x 5 3))

  (lisp-unit:assert-eq 52
		       (x 13 4))

  (lisp-unit:assert-equal '(11 11 11 11 11)
			  (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))

  (lisp-unit:assert-equal '(6 9)
			  (tup+ '(2 3) '(4 6)))

  (lisp-unit:assert-equal '(7 13 8 1)
			  (tup+ '(3 7) '(4 6 8 1)))

  (lisp-unit:assert-equal '(7 13 8 1)
			  (tup+ '(3 7 8 1) '(4 6)))
  
  (lisp-unit:assert-false (greater-than 12 133))

  (lisp-unit:assert-true (greater-than 120 11))

  (lisp-unit:assert-false (greater-than 3 3))

  (lisp-unit:assert-true (less-than 4 6))

  (lisp-unit:assert-false (less-than 8 3))

  (lisp-unit:assert-false (less-than 6 6))

  (lisp-unit:assert-true (our-equal 6 6))

  (lisp-unit:assert-false (our-equal 6 5))

  (lisp-unit:assert-eq 1 (our-expt 1 1))

  (lisp-unit:assert-eq 8 (our-expt 2 3))

  (lisp-unit:assert-eq 125 (our-expt 5 3))

  (lisp-unit:assert-eq 3 (integer-division 15 4))

  (lisp-unit:assert-eq 6 (our-length '(hotdogs with mustard sauerkraut
  and pickles)))

  (lisp-unit:assert-eq 5 (our-length '(ham and cheese on rye)))

  (lisp-unit:assert-eq 'macaroni (our-pick 4 '(lasagna spaghetti ravioli
				       macaroni meatball)))

  (lisp-unit:assert-equal '(hotdogs with mustard)
			  (rempick 3 '(hotdogs with hot mustard)))

  (lisp-unit:assert-equal '(hotdogs with mustard)
			  (rempick-using-one 3 '(hotdogs with hot
					     mustard)))
  
  (lisp-unit:assert-false (numberp 'tomato))

  (lisp-unit:assert-true (numberp 76))

  (lisp-unit:assert-equal
   '(pears prunes dates)
   (no-nums '(5 pears 6 prunes 9 dates)))

  (lisp-unit:assert-equal
   '()
   (no-nums '(5 6 9)))

  (lisp-unit:assert-equal
   '(pears prunes dates)
   (no-nums '(pears prunes dates)))
  
  (lisp-unit:assert-equal '(5 6 9)
   (all-nums '(5 pears 6 prunes 9 dates)))

  (lisp-unit:assert-equal '(5 6 9)
   (all-nums '(5 6 9)))

  (lisp-unit:assert-equal '()
   (all-nums '(pears prunes dates)))
  
  (lisp-unit:assert-true (eqan 'a 'a))

  (lisp-unit:assert-true (eqan 3 3))

  (lisp-unit:assert-false (eqan 'a '(a)))

  (lisp-unit:assert-false (eqan '3 '(a)))

  (lisp-unit:assert-false (eqan 3 'a))

  (lisp-unit:assert-eq 3 (occur 'pizza '(pizza macaroni lasagna pizza
				pizza salade))) 

  (lisp-unit:assert-eq 0 (occur 'hotdogs '(pizza macaroni lasagna
				pizza pizza salade)))

  (lisp-unit:assert-true (onep 1))

  (lisp-unit:assert-false (onep 0))

  (lisp-unit:assert-false (onep 50))
  
  )