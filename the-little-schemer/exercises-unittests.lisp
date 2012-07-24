(in-package :com.github.massimo-nocentini.the-little-schemer)


(lisp-unit:define-test  chapter-four-exercises

  (assert-equal '((x y) (x y) (x y)) (duplicate 3 '(x y)) )
  (assert-equal '() (duplicate 0 '(x y)) )
  (assert-equal '((1 2)) (duplicate 1 '(1 2)) )

  (assert-eql 3 (index 'car '(cons cdr car null eq)))
  (assert-eql 1 (index 'car '(car engine auto motor)))
  (assert-eql nil (index 'car '()))
  (assert-eql 4 (index 'motor '(car engine auto motor)))  

  (assert-eql 3 (index-with-acc 'car '(cons cdr car null eq) 0))
  (assert-eql 1 (index-with-acc 'car '(car engine auto motor) 0))
  (assert-eql nil (index-with-acc 'car '() 0))
  (assert-eql 4 (index-with-acc 'motor '(car engine auto motor) 0))

  (assert-eql 29 (dot-product-with-acc '(3 2 4) '(3 2 4) 0))
  (assert-eql 26 (dot-product-with-acc '(3 2 4) '(6 2 1) 0))
  (assert-eql 17 (dot-product-with-acc '(2 1 3) '(6 2 1) 0))

  (assert-equal '((bananas) (kiwis))
		(multidown-with-acc '(bananas kiwis) '()))
  (assert-equal '((peaches) (apples) (bananas))
		(multidown-with-acc '(peaches apples bananas) '()))
  (assert-equal '() (multidown-with-acc '() '()))

  (assert-equal '() (multiup-with-acc '() '()))
  (assert-equal '(curry chicken)
		(multiup-with-acc '((curry) () (chicken) ()) '()))
  (assert-equal '(peaches (and cream))
		(multiup-with-acc '((peaches) (and cream)) '()))
  
  
  )

(lisp-unit:define-test chapter-five-exercises

  (assert-equal '((((a)) (b) ((c) (d))) (d))
  		(down*-with-acc-prefix '(((a) b (c d)) d) '()))

  (assert-equal '() (down*-with-acc-prefix '() '()))

  (assert-equal '((chili) (and) (hot))
  		(down*-with-acc-prefix '(chili and hot) '()))

  (assert-equal '((((a)) (b) ((c) (d))) (d))
  		(down*-with-acc-postfix '(((a) b (c d)) d)
					'()))

  (assert-equal '() (down*-with-acc-postfix '() '()))

  (assert-equal '((chili) (and) (hot))
  		(down*-with-acc-postfix '(chili and hot) '()))
  
  (assert-equal '((((a)) (b) ((c) (d))) (d))
		(down* '(((a) b (c d)) d)))

  (assert-equal '() (down* '()))

  (assert-equal '((chili) (and) (hot))
		(down* '(chili and hot)))

  (assert-equal '((a b ((d) c)) d)
		(up* '(((a) b (((d)) (c))) () d)))

  (assert-equal '((a b ((d) c)) d)
		(up*-with-acc-prefix
		 '(((a) b (((d)) (c))) () d) '()))

  (assert-equal '(((())))
		(up*-with-acc-prefix
		 '((() ((()) ())) ()) '()))

  (assert-eql 4 (occur*-with-acc 'a
				 '((((n (b ((a)))) a)) a (a)) 0))
  (assert-eql 0 (occur*-with-acc 'j
				 '((((a (b ((a)))) a)) a (a)) 0))
  
  )