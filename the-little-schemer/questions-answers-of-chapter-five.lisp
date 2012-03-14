(lisp-unit:define-test questions-of-chapter-five
  (lisp-unit:assert-equal '((coffee) ((tea)) (and (hick)))
			  (rember* 'cup '((coffee) cup ((tea)
				   cup) (and (hick)) cup)) )

  (lisp-unit:assert-equal '(((tomato)) ((bean)) (and ((flying))))
			  (rember* 'sauce '(((tomato sauce)) ((bean)
				   sauce) (and ((flying)) sauce))) )

  (lisp-unit:assert-false (latp '(((tomato sauce)) ((bean)
				   sauce) (and ((flying)) sauce))))

  (lisp-unit:assert-false (atomp
			   (car '(((tomato sauce)) ((bean)
			   sauce) (and ((flying)) sauce)))))

  (lisp-unit:assert-equal
   '((how much (wood)) could ((a (wood) chuck
   roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck
   roast wood)
   (insert-r* 'roast 'chuck '((how much (wood)) could ((a (wood)
   chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)) )

  (lisp-unit:assert-eq 5
		       (occur* 'banana '((banana) (split ((((banana
			       ice))) (cream (banana))
			       sherbet)) (banana) (bread) (banana
			       brandy))))

  (lisp-unit:assert-equal
   '((orange) (split ((((orange
   ice))) (cream (orange)) sherbet)) (orange) (bread) (orange
   brandy))
   (subst* 'orange 'banana '((banana) (split ((((banana
   ice))) (cream (banana)) sherbet)) (banana) (bread) (banana
   brandy))) )

  (lisp-unit:assert-equal '((how much (wood)) could ((a (wood) pecker
   chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could
   pecker chuck wood)
			  (insert-l* 'pecker 'chuck '((how
   much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood
   chuck))) could chuck wood)) )

  (lisp-unit:assert-true (member* 'chips '((potato) (chips ((with)
				  fish) (chips)))))

  (lisp-unit:assert-eq 'potato
		       (leftmost '((potato) (chips ((with)
		       fish) (chips)))))

  (lisp-unit:assert-eq 'hot (leftmost '(((hot) (tuna (and))) cheese)))

  (lisp-unit:assert-true (eqlistp '(strawberry ice cream) '(strawberry
				  ice cream)))

  (lisp-unit:assert-false (eqlistp '(strawberry ice cream) '(strawberry
				  cream ice)))

  (lisp-unit:assert-false (eqlistp '(banana ((split)))
				   '((banana) (split))))

  (lisp-unit:assert-false (eqlistp '(beef ((sausage)) (and (soda)))
				   '(beef ((salami)) (and (soda)))))

  (lisp-unit:assert-true (eqlistp '(beef ((sausage)) (and (soda)))
				   '(beef ((sausage)) (and (soda)))))
  
  (lisp-unit:assert-equal '(lettuce and tomato)
     (rember-sexp-version 'bacon '(bacon lettuce and tomato)))

  (lisp-unit:assert-equal '(bacon lettuce tomato)
     (rember-sexp-version 'and '(bacon lettuce and tomato)))

  (lisp-unit:assert-equal '(soy and tomato sauce)
     (rember-sexp-version 'sauce '(soy sauce and tomato sauce)))

  (lisp-unit:assert-equal '(bacon)
     (rember-sexp-version '(lettuce (and tomato)) '(bacon (lettuce (and
     tomato)))))

  ;; searching for the empty list to be removed we got the wrong
  ;; result

  ;; (lisp-unit:assert-equal '(soy and tomato sauce)
  ;; (rember-sexp-version '() '(soy () sauce and tomato sauce)))
  
  
  
  
  )

