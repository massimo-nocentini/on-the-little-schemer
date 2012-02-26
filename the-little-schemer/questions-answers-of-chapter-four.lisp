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
  


  )