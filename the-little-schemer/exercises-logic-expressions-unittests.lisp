(in-package :com.github.massimo-nocentini.the-little-schemer)


(lisp-unit:define-test logic-expressions

  (lisp-unit:assert-true (lexp? '(and (or x y o) y u (not i))))
  (lisp-unit:assert-true (lexp? '(and y u  i)))
  (lisp-unit:assert-true (lexp? '()))
  (lisp-unit:assert-true (lexp? '(NOT (OR e r))))
  (lisp-unit:assert-false (lexp? '(NOT x (AND u i))))
  (lisp-unit:assert-false (lexp? '(1 + (3 x 4))))
  (lisp-unit:assert-false (lexp? '(3 + (66 6))))

  (lisp-unit:assert-true (covered?
			  '()
			  '(x y o y u i)))  
  (lisp-unit:assert-true (covered?
			  '(and (or x y o) y u (not i))
			  '(x y o u i)))
  (lisp-unit:assert-false (covered?	;Y missing
			  '(and (or x y o) y u (not i))
			  '(x o u i)))
  


  )
