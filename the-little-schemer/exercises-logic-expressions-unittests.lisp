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

  (lisp-unit:assert-eql 1 (lookup 'a '((a 1) (b 0))))
  (lisp-unit:assert-eql 0 (lookup 'b '((a 1) (b 0))))
  (lisp-unit:assert-eq nil (lookup 'c '((a 1) (b 0))))
  
  (lisp-unit:assert-equal '(a b)
			  (extract-variables '((a 1) (b 0))))
  (lisp-unit:assert-equal '(a b a)
			  (extract-variables
			   '((a 1) (b 0) (a 0))))
  (lisp-unit:assert-equal '(b)
			  (extract-variables '((b 0))))
  (lisp-unit:assert-equal '()
			  (extract-variables '()))  

  (lisp-unit:assert-true (meaning-of-lexp
			  '(and (or x y o) y u (not i))
			  '((x t) (y t) (o nil) (u t) (i nil))))
  (lisp-unit:assert-false (meaning-of-lexp
			   '(and y u i)
			   '((y t) (u nil) (i t))))
  (lisp-unit:assert-false (meaning-of-lexp '() '()))
  (lisp-unit:assert-true (meaning-of-lexp
			  '(NOT (OR e r))
			  '((e nil) (r nil))))
  (lisp-unit:assert-eq 'not-covered	;E not covered
		       (meaning-of-lexp
			  '(NOT (OR e r))
			  '((a nil) (r nil))))  

  )
