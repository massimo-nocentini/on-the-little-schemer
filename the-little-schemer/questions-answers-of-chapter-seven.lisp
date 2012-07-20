(in-package :com.github.massimo-nocentini.the-little-schemer)

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

  (lisp-unit:assert-equal '(a b c) (tls-set-difference '(a b c) '(d e
						  f g)))

  (lisp-unit:assert-equal '(h d) (tls-set-difference '(a h b d) '(c a
						     j k b)))

  (lisp-unit:assert-equal '(a) (intersect-all '((a b c)
						(c a d e)
						(e f g h a b))))

  (lisp-unit:assert-equal '(6 and) (intersect-all '((6 pears and)
						    (3 peaches and 6 peppers)
						    (8 pears and 6 plums)
						    (and 6 prunes with
						    some apples))))

  (lisp-unit:assert-true (pairp '(pear pear)))

  (lisp-unit:assert-true (pairp '(3 7)))

  (lisp-unit:assert-true (pairp '((2) (pair))))

  (lisp-unit:assert-true (pairp '(full (house))))

  (lisp-unit:assert-false (pairp 4))

  (lisp-unit:assert-false (pairp 'r))

  (lisp-unit:assert-false (pairp '()))

  (lisp-unit:assert-false (pairp '(a)))

  (lisp-unit:assert-false (pairp '(a g a)))

  (lisp-unit:assert-equal 'h (pair-first-component '(h (some cdr))))

  (lisp-unit:assert-equal '(some cdr) (pair-second-component '(h (some
				       cdr))))

  (lisp-unit:assert-equal '(first-component (second components))
			  (build-pair 'first-component '(second components)))

  (lisp-unit:assert-false (relationp '(apples peaches pumpkin pie)))

  (lisp-unit:assert-false (relationp '((apples peaches)
				       (pumpkin pie)
				       (apples peaches))))

  (lisp-unit:assert-true (relationp '((apples peaches)
				      (pumpkin pie))))

  (lisp-unit:assert-true (relationp '((4 3)
				      (4 2)
				      (7 6)
				      (6 2)
				      (3 4))))

  (lisp-unit:assert-false (tls-functionp '((4 3)
				      (4 2)
				      (7 6)
				      (6 2)
				      (3 4))))

  (lisp-unit:assert-true (tls-functionp '((8 3)
				      (4 2)
				      (7 6)
				      (6 2)
				      (3 4))))

  (lisp-unit:assert-false (tls-functionp '((d 4)
				      (b 0)
				      (b 9)
				      (e 5)
				      (g 4))))

  (lisp-unit:assert-equal '((8 a) (pumpkin pie) (got sick))
			  (revrel '((a 8) (pie pumpkin) (sick got))))

  (lisp-unit:assert-false (fullfunp '((8 3)
				      (4 2)
				      (7 6)
				      (6 2)
				      (3 4))))

  (lisp-unit:assert-true (fullfunp '((8 3)
				      (4 8)
				      (7 6)
				      (6 2)
				      (3 4))))

  (lisp-unit:assert-false (fullfunp '((grape raisin)
				      (plum prune)
				      (stewed prune))))

  (lisp-unit:assert-true (fullfunp '((grape raisin)
				      (plum prune)
				      (stewed grape))))



  )

