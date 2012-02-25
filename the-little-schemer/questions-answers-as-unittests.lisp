
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-unit "lisp-unit.lisp"))
  

(lisp-unit:define-test simple-atom-questions-chapter-one
  (lisp-unit:assert-false
   (atomp (quote ())) )

  ;; this assert, respect the previous one, use the built in atom
  ;; function, which differs from the atomp that is used in the book
  (lisp-unit:assert-true
   (atom (quote ())) )

  (lisp-unit:assert-true
   (atomp 'atom) )

  (lisp-unit:assert-true
   (atomp 'turkey) )

  (lisp-unit:assert-true
   (atomp 1492) )

  (lisp-unit:assert-true
   (atomp 'u) )

  (lisp-unit:assert-true
   (atomp '*abc$) )

  (lisp-unit:assert-true
   (listp '(atom)) )

  (lisp-unit:assert-true
   (listp '(atom turkey or)) )

  (lisp-unit:assert-true
   (listp '((atom turkey) or)) )

  (lisp-unit:assert-true
   (listp '(how are you doing so far)) )

  (lisp-unit:assert-true
   (listp '()) )

  (lisp-unit:assert-true
   (listp '(() () () ()) ) )
  )

(lisp-unit:define-test simple-car-cdr-questions-chapter-one
  (lisp-unit:assert-eq
   'a
   (car '(a b c)) )

  ;; before we procede with the question we have to point out that
  ;; there no exists the (car a) where a is an atom.
  
  ;; the following test show that the car return the first element
  ;; itself and not copy of it ('cause we use assert-eq instead of
  ;; assert-equal
  (lisp-unit:assert-eq
   '(a b c)
   (car '((a b c) x y z)) )

  (lisp-unit:assert-eq
   '()
   (car '()) )

  (lisp-unit:assert-eq
   '((hotdogs))
   (car '(((hotdogs)) (and) (pickle) relish)) )
  
  (lisp-unit:assert-eq
   '(hotdogs)
   (car (car '(((hotdogs)) (and)) ) ) )

  ;; the following test shows that the cdr build a completely new list
  ;; (or better a new cons) to make its return object., hence we must
  ;; use the assert-equal which check the element of the list and not
  ;; the entire list object (like assert-eq does)
  (lisp-unit:assert-equal
   '(b c)
   (cdr '(a b c)) )

  (lisp-unit:assert-equal
   '(x y z)
   (cdr '((a b c) x y z)) )  

  ;; here the interesting thing is that we use the cdr which, as shown
  ;; in the assertions above, build a new list, but applying it to a
  ;; list with only one element inside, we get the () which we can
  ;; compare with eq predicate and it returns true. However, as shown
  ;; in the assertion below, it is coherent with the predicate equal.
  (lisp-unit:assert-eq
   '()
   (cdr '(hamburger)) )  

  ;; this assertion is correlated to the previous one, read its
  ;; comment.
  (lisp-unit:assert-equal
   '()
   (cdr '(hamburger)) )

  (lisp-unit:assert-equal
   '(t r)
   (cdr '((x) t r)) )

  (lisp-unit:assert-false
   (equal '(r t)
   (cdr '((x) t r))) )


  
  
  )
