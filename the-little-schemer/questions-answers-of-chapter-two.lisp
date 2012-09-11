(in-package :com.github.massimo-nocentini.the-little-schemer)

(define-test questions-about-lat-and-memberp
  (lisp-unit:assert-true
   (latp (quote (bacon and eggs))) )

  (lisp-unit:assert-false
   (latp (quote (bacon '(and eggs)))) )

  (lisp-unit:assert-true
   (latp ()))

  (lisp-unit:assert-true
   (or (null ()) (atomp '(d e f g))))

  (lisp-unit:assert-true
   (or (null '(a b c)) (null ())))

  (lisp-unit:assert-true
   (memberp 'tea '(coffee tea or milk)))

  (lisp-unit:assert-false
   (memberp 'poached '(fried eggs and scrambled eggs)))

  (lisp-unit:assert-true
   (memberp 'meat '(mashed potatoes and meat gravy)))

  (lisp-unit:assert-true
   (memberp 'meat '(and meat gravy)))

  (lisp-unit:assert-true
   (memberp 'meat '(potatoes and meat gravy)))

  (lisp-unit:assert-true
   (memberp 'meat '(mashed potatoes and meat gravy)))

  (lisp-unit:assert-false
   (memberp 'liver '(bagels and lox)))

  (lisp-unit:assert-true
   (nonlatp '((blue chhese) (and) (red) (wine))))

  (lisp-unit:assert-false
   (nonlatp '()))

  (lisp-unit:assert-false
   (nonlatp '(atomic-sexp (eggs))))

  (lisp-unit:assert-eq
   (memberp 'coffee '(german chocolate cake))
   (member2p 'coffee '(german chocolate cake)))

  (lisp-unit:assert-eq
   (memberp 'coffee '(poppy seed cake))
   (member2p 'coffee '(poppy seed cake)))

  (lisp-unit:assert-eq
   (memberp 'seed '(poppy seed cake))
   (member2p 'seed '(poppy seed cake)))

  (lisp-unit:assert-false
   (memberp-lists-inside-guarded 'coffee '((linzer) (torte) ())))

  (lisp-unit:assert-true
   (memberp-lists-inside-guarded 'coffee '((linzer) (torte) ()
   coffee)))

  (lisp-unit:assert-true
   (member-twice 'egg '(food egg potato egg)))

  (assert-true
   (member-twice 'egg '(food egg potato egg tomato egg)))

  (assert-false
   (member-twice 'egg '(food potato)))

  (assert-false
   (member-twice 'egg '()))
  
  )


