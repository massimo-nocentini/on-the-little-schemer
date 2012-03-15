(lisp-unit:define-test questions-of-chapter-seven

  (lisp-unit:assert-false (setp '(apple peaches apple plum)))
  (lisp-unit:assert-true (setp '(apples peaches pears plums)))
  (lisp-unit:assert-true (setp '()))

  (lisp-unit:assert-equal '(pear plum apple lemon peach)
			  (makeset '(apple peach pear peach plum apple
			  lemon peach)))

  (lisp-unit:assert-equal '(apple peach pear plum lemon)
			  (makeset-variation '(apple peach pear peach
			  plum apple lemon peach)))

  (lisp-unit:assert-equal '(apple 3 pear 4 9)
			  (makeset-variation '(apple 3 pear 4 9 apple
			   3 4)))

  (lisp-unit:assert-true (tls-subsetp '(5 chicken wings) '(5 hamburgers 2
				  pieces fried chicken and light
				  duckling wings)))

  (lisp-unit:assert-false (tls-subsetp '(4 punds of horseradish)
				       '(four pounds chicken and 5
				       onces horseradish)))

  (lisp-unit:assert-true (eqsetp '(6 large chickens with wings) '(6
				 chickens with large wings)))

  (lisp-unit:assert-true (intersectp '(stewed tomatoes and macaroni)
				     '(macaroni and cheese)))

  (lisp-unit:assert-equal '(and macaroni)
			  (intersect '(stewed tomatoes and macaroni)
				     '(macaroni and cheese)))

  (lisp-unit:assert-equal '(stewed tomatoes casserole macaroni and cheese)
			  (tls-union '(stewed tomatoes and macaroni
				 casserole) '(macaroni and cheese)))

  )

