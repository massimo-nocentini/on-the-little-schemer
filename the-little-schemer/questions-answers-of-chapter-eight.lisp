(in-package :com.github.massimo-nocentini.the-little-schemer)

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

  (lisp-unit:assert-equal '(shrimp salad salad and)
			  (funcall (multi-rember-f (function eq))
				   (quote tuna) '(shrimp salad tuna
				   salad and tuna)))

  (lisp-unit:assert-equal '(shrimp salad salad and)
			  (multi-rember-t (eq?-c 'tuna) '(shrimp salad
					  tuna salad and tuna)))

  (lisp-unit:assert-false
   (multi-rember&co (quote tuna) (quote (strawberries tuna and
					 swordfish))
		    (lambda (newl seen)	;these are the results of the
					;entire recursive computation
		      (null seen) ;this is the return value of the
				  ;entire application of function
				  ;multi-rember&co
		      )))

  (lisp-unit:assert-true
   (multi-rember&co (quote tuna) (quote ())
		    (lambda (newl seen)	;these are the results of the
					;entire recursive computation
		      (null seen) ;this is the return value of the
				  ;entire application of function
				  ;multi-rember&co
		      )))

  (lisp-unit:assert-false
   (multi-rember&co (quote tuna) (quote (tuna))
		    (lambda (newl seen)	;these are the results of the
					;entire recursive computation
		      (null seen) ;this is the return value of the
				  ;entire application of function
				  ;multi-rember&co
		      )))

  (lisp-unit:assert-false
   (multi-rember&co (quote tuna) (quote (and tuna))
		    (lambda (newl seen)	;these are the results of the
					;entire recursive computation
		      (null seen) ;this is the return value of the
				  ;entire application of function
				  ;multi-rember&co
		      )))

  (lisp-unit:assert-eq 3
   (multi-rember&co (quote tuna) (quote (strawberries tuna and swordfish))
		    (lambda (newl seen)	;these are the results of the
					;entire recursive computation
		      (length newl) ;this is the return value of the
				  ;entire application of function
				  ;multi-rember&co
		      )))

  (lisp-unit:assert-equal (quote (chips salty and salty fish or salty
					fish and chips salty))
			  (multi-insert-lr (quote salty)
					   (quote fish)
					   (quote chips)
					   (quote (chips and fish or
							 fish and
							 chips))) )

  (lisp-unit:assert-equal (quote (2 2 chips salty and salty fish or salty
					fish and chips salty))
			  (multi-insert-lr&co (quote salty)
					   (quote fish)
					   (quote chips)
					   (quote (chips and fish or
							 fish and
							 chips))
					   (lambda (newlat l r)
					     (cons l (cons r newlat)))) )

  (lisp-unit:assert-equal (quote ((2 8) 10 (() 6) 2))
			  (evens-only* (quote ((9 1 2 8) 3 10 ((9 9) 7
					       6) 2))))
  
  (lisp-unit:assert-equal (quote (38 1920 (2 8) 10 (() 6) 2))
			  (evens-only*&co (quote ((9 1 2 8) 3 10 ((9 9) 7
					       6) 2))
					  (lambda (l odd-prod even-sum)
					    (cons even-sum
						  (cons odd-prod
						  l)))))

  (lisp-unit:assert-equal (quote (3 tea see Cecina))
			  (funcall (multi-rember-single-param (eq?-c 'spider))
				   (quote (3 spider tea see spider
				   Cecina))))
  
  
  
  )