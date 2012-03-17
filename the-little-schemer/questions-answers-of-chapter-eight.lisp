(lisp-unit:define-test questions-of-chapter-eight

  (lisp-unit:assert-equal '(6 2 3)
			  (funcall (rember-f
				    (function our-equal)) 5 '(6 2 5
				    3)))

  (lisp-unit:assert-equal '(beans are good)
			  (funcall (rember-f (function eq))
				    'jelly
				    '(jelly beans are good)))

  (lisp-unit:assert-equal '(lemonade and (cake))
			  (funcall (rember-f (function equal-sexps))
				    '(pop corn) '(lemonade (pop corn)
				    and (cake))))

  (lisp-unit:assert-true (funcall (eq?-c 'salad) 'salad))

  (lisp-unit:assert-false (funcall (eq?-c 'salad) 'tuna))

  (lisp-unit:assert-equal '(equal? eqan? eqlist? eqpair?)
			  (funcall (rember-f (function eq)) 'eq?
				   '(equal? eq? eqan? eqlist?
				   eqpair?)))

  (lisp-unit:assert-equal '(salad new macaroni the)
			  (funcall (insert-l-f (function eq))
				   'new 'macaroni
				   '(salad macaroni the)))

  (lisp-unit:assert-equal '(salad macaroni new the)
			  (funcall (insert-r-f (function eq))
				   'new 'macaroni
				   '(salad macaroni the)))

  (lisp-unit:assert-equal '(salad new the)
			  (funcall (subst-f (function eq))
				   'new 'macaroni
				   '(salad macaroni the)))
  
  (lisp-unit:assert-equal '(equal? eqan? eqlist? eqpair?)
  			  (funcall (rember-f-final (function eq))
				   nil ;this NIL parameter is not used
				       ;effectively, is only there
				       ;because the lambda returned by
				       ;rember-f-final takes three
				       ;parameters
				   'eq?  '(equal? eq? eqan? eqlist?
				   eqpair?)))
  
  )