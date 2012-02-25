
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-unit "lisp-unit.lisp"))
  

(lisp-unit:define-test questions-about-lat-and-memberp
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

  )


