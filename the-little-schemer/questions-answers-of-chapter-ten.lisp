(lisp-unit:define-test questions-of-chapter-ten

  (lisp-unit:assert-equal '((first-name second-name)
			    (first-value second-value))
			  (new-entry '(first-name second-name)
				     '(first-value second-value)))
  
    (lisp-unit:assert-eq 'tastes
			 (lookup-in-entry 'entree '((appetizer entree
					  beverage) (food tastes
					  good))
					  (lambda (missing-name)
					    `(element ,missing-name
					    not found))))

    (lisp-unit:assert-equal '(element peperoni not found)
			    (lookup-in-entry 'peperoni '((appetizer
					  entree beverage) (food
					  tastes good))
					  (lambda (missing-name)
					    `(element ,missing-name
					    not found))))

    (lisp-unit:assert-equal '(((appetizer entree beverage)
			       (pate boeuf vin))
			      ((beverage dessert)
			      ((food is) (number one with us))) )
			    (extend-table '((appetizer entree beverage)
					    (pate boeuf vin))
					  '(((beverage dessert)
					     ((food is) (number one
					     with us))))))

    (lisp-unit:assert-eq 'spaghetti
			 (lookup-in-table 'entree
					  '(((entree dessert)
					     (spaghetti spumoni))
					    ((appetizer entree beverage)
					     (food tastes good)))
					  (lambda (missing-name)
					    `(element ,missing-name
					    not found))))

    (lisp-unit:assert-equal
     '(element peperoni not found)
     (lookup-in-table 'peperoni
		      '(((entree dessert)
			 (spaghetti spumoni))
			((appetizer entree beverage)
			 (food tastes good)))
		      (lambda (missing-name)
			`(element ,missing-name not found))))

    (lisp-unit:assert-equal '(a b c)
			    (cons 'a (cons 'b (cons 'c (quote ())))))

    (lisp-unit:assert-equal '(car (quote (a b c)))
			    (cons (quote car)
				  (cons
				   (cons (quote quote)
					 (cons
					  (cons 'a
						(cons 'b
						      (cons 'c
							    (quote ()))))
					  (quote ())))
				   (quote ()))))

    (lisp-unit:assert-eq 'a
			 (car (quote (a b c))))

    (lisp-unit:assert-eq '(car (quote (a b c)))
			 (tls-eval (quote (car (quote (a b c))))))

    (lisp-unit:assert-eq 7
			 (tls-eval (quote (1+ 6))))

    (lisp-unit:assert-eq 6
			 (tls-eval 6))

    (lisp-unit:assert-eq 'nothing
			 (tls-eval (quote nothing)))

    (lisp-unit:assert-equal '((from nothing comes something))
			    (tls-eval (quote ((lambda (nothing)
						(cons nothing (quote ())))
					      (quote (from nothing
					      comes something))))))

    (lisp-unit:assert-eq 'something
			 (tls-eval (quote ((lambda (nothing)
					     (cond
					       (nothing (quote something))
					       (t (quote nothing))))
					   (quote t)))))

    (lisp-unit:assert-false (tls-eval ()))

    (lisp-unit:assert-equal '(primitive car)
			    (tls-eval (quote car)))

    (lisp-unit:assert-equal )
    

)