
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :lisp-unit "lisp-unit.lisp"))
  

(lisp-unit:define-test questions-about-rember
  (lisp-unit:assert-equal '(lettuce and tomato)
     (rember 'bacon '(bacon lettuce and tomato)))

  (lisp-unit:assert-equal '(bacon lettuce tomato)
     (rember 'and '(bacon lettuce and tomato)))

  (lisp-unit:assert-equal '(soy and tomato sauce)
     (rember 'sauce '(soy sauce and tomato sauce)))

  (lisp-unit:assert-equal '(apple plum grape bean)
			  (firsts '(
				    (apple peach pumpkin)
				    (plum pear cherry)
				    (grape raisin pea)
				    (bean carrot eggplant))))

  (lisp-unit:assert-equal '(a c e)
			  (firsts '(
				    (a b)
				    (c d)
				    (e f))))

  (lisp-unit:assert-equal '()
			  (firsts '()))

  (lisp-unit:assert-equal '(five four eleven)
			  (firsts '(
				    (five plums)
				    (four)
				    (eleven green oranges) )))

  (lisp-unit:assert-equal '((five plums) eleven (no))
			  (firsts '(
				    ((five plums) four)
				    (eleven green oranges)
				    ((no) more))))

  (lisp-unit:assert-equal '(ice cream with fudge topping for dessert)
			  (insertR 'topping
				   'fudge
				   '(ice cream with fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales and jalapeno salsa)
			  (insertR 'jalapeno
				   'and
				   '(tacos tamales and salsa)))

  (lisp-unit:assert-equal '(a b c d e f g h)
			  (insertR 'e 'd '(a b c d f g h)))

  (lisp-unit:assert-equal '(ice cream with topping fudge for dessert)
			  (insertL 'topping
				   'fudge
				   '(ice cream with fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales jalapeno and salsa)
			  (insertL 'jalapeno
				   'and
				   '(tacos tamales and salsa)))

  (lisp-unit:assert-equal '(a b c e d f g h)
			  (insertL 'e 'd '(a b c d f g h)))
  
  

  )
				    