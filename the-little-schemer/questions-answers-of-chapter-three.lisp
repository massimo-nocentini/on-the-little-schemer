(in-package :com.github.massimo-nocentini.the-little-schemer)

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

  (lisp-unit:assert-equal '(a b c d f g h)
			  (insertR 'e 'm '(a b c d f g h)))
  
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

  (lisp-unit:assert-equal '(a b c d f g h)
			  (insertL 'e 'm '(a b c d f g h)))

  (lisp-unit:assert-equal '(ice cream with topping for dessert)
			  (my-subst 'topping 'fudge '(ice cream with
				   fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales jalapeno salsa)
			  (my-subst 'jalapeno 'and '(tacos tamales and
				   salsa)))

  (lisp-unit:assert-equal '(a b c e f g h)
			  (my-subst 'e 'd '(a b c d f g h)))

  (lisp-unit:assert-equal '(a b c d f g h)
			  (my-subst 'e 'm '(a b c d f g h)))
  
  (lisp-unit:assert-equal '(vanilla ice cream with chocolate topping)
			  (my-subst2 'vanilla 'chocolate 'banana
				     '(banana ice cream with chocolate
				     topping)))

  (lisp-unit:assert-equal '(coffee tea and hick)
			  (multirember 'cup '(coffee cup tea cup and
			  hick cup)))

  (lisp-unit:assert-equal '(ice fudge topping cream with fudge topping
   for dessert)
			  (multi-insert-r 'topping
				   'fudge
				   '(ice fudge cream with fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales and jalapeno salsa)
			  (multi-insert-r 'jalapeno 'and '(tacos
					  tamales and salsa)))

  (lisp-unit:assert-equal '(a b c d e f g h)
			  (multi-insert-r 'e 'd '(a b c d f g h)))

  (lisp-unit:assert-equal '(a b c d f g h)
			  (multi-insert-r 'e 'm '(a b c d f g h)))

  (lisp-unit:assert-equal '(coffee cup new tea cup new and hick cup
   new)
			  (multi-insert-r 'new 'cup '(coffee cup tea
					  cup and hick cup)))

  (lisp-unit:assert-equal '(ice topping fudge cream with topping fudge
   for dessert)
			  (multi-insert-l 'topping
				   'fudge
				   '(ice fudge cream with fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales jalapeno and salsa)
			  (multi-insert-l 'jalapeno 'and '(tacos
					  tamales and salsa)))

  (lisp-unit:assert-equal '(a b c e d f g h)
			  (multi-insert-l 'e 'd '(a b c d f g h)))

  (lisp-unit:assert-equal '(a b c d f g h)
			  (multi-insert-l 'e 'm '(a b c d f g h)))

  (lisp-unit:assert-equal '(coffee new cup tea new cup and hick new
   cup)
			  (multi-insert-l 'new 'cup '(coffee cup tea
					  cup and hick cup)))  

  (lisp-unit:assert-equal '(ice cream with topping for dessert)
			  (multi-subst 'topping 'fudge '(ice cream
				   with fudge for dessert)))
  
  (lisp-unit:assert-equal '(tacos tamales jalapeno salsa)
			  (multi-subst 'jalapeno 'and '(tacos tamales
				   and salsa)))

  (lisp-unit:assert-equal '(a e b c e f g h)
			  (multi-subst 'e 'd '(a d b c d f g h)))

  (lisp-unit:assert-equal '(a b c d f g h)
			  (my-subst 'e 'm '(a b c d f g h)))


  
  )

(lisp-unit:define-test chapter-three-exercises-from-the-little-lisper

  (lisp-unit:assert-equal '(spanish red beans)
     (seconds '((paella spanish) (wine red) (and beans))))

  (lisp-unit:assert-equal '()
     (seconds '()))

  (lisp-unit:assert-equal '(hot dogs)
     (seconds '((and hot) (but dogs))))

  )
				    