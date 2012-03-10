(lisp-unit:define-test questions-of-chapter-five
  (lisp-unit:assert-equal '((coffee) ((tea)) (and (hick)))
			  (rember* 'cup '((coffee) cup ((tea)
				   cup) (and (hick)) cup)) )

  (lisp-unit:assert-equal '(((tomato)) ((bean)) (and ((flying))))
			  (rember* 'sauce '(((tomato sauce)) ((bean)
				   sauce) (and ((flying)) sauce))) )

  (lisp-unit:assert-false (latp '(((tomato sauce)) ((bean)
				   sauce) (and ((flying)) sauce))))


  )

