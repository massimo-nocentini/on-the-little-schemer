
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

  (lisp-unit:assert-false
   (eq '(b c)
       (cdr '(a b c))) )  

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

  (lisp-unit:assert-error
   'type-error
   (cdr 'hotdogs))
  
  (lisp-unit:assert-eq
   '()
   (cdr '() ) )

  ;; this assertion is the prelude of the next one.
  (lisp-unit:assert-eq
   '(x y)
   '(x y))
  
  ;; this is quite strange because in a first place we apply cdr which
  ;; build a new cons (as we observed some assertion above) and after
  ;; we apply the car on the temporary list, and the two list are
  ;; equal and equivalent, I expected only equivalent. After some
  ;; thoughts, only the conses are built from scratch, but their
  ;; content is the same, and the content is what is returned by the
  ;; car, that is not the cons structure which, in turn, is returned
  ;; by the cons application.  We've used the assert-eq because of the
  ;; application of the car, which return the content, so we are in
  ;; the case of the assertion above.
  (lisp-unit:assert-eq
   '(x y)
   (car (cdr '((b) (x y) ((c))) )) )

  (lisp-unit:assert-equal
   '(((c)))
   (cdr (cdr '((b) (x y) ((c))) )) )

  (lisp-unit:assert-false
   (eq '(((c)))
       (cdr (cdr '((b) (x y) ((c))) ))) )

  (lisp-unit:assert-error
   'type-error				;to get this atom I've first
					;simulated the following
					;expression in the REPL
   (cdr (car '(a (b (c)) d) ) ))
    
  )

(lisp-unit:define-test cons-questions-chapter-one
  (lisp-unit:assert-equal		;we have to use the equal
					;predicate because cons return
					;a structure not the content
					;(like car does)
   '(peanut butter and jelly)
   ;; read the following one as "cons the atom 'peanut onto the list"
   (cons 'peanut '(butter and jelly)) )

  (lisp-unit:assert-equal
   '((banana and) peanut butter and jelly)
   (cons '(banana and) '(peanut butter and jelly)))

  (lisp-unit:assert-equal
   '(((help) this) is very ((hard) to learn))
   (cons '((help) this) '(is very ((hard) to learn))))

  (lisp-unit:assert-equal
   '((a b (c)))
   (cons '(a b (c)) ()) )

  (lisp-unit:assert-equal
   '(a)
   (cons 'a ()))

  (lisp-unit:assert-equal		;also for the 'dotted' cons we
					;have to use the equal
					;predicate instead of eq,
					;because using cons for this
					;special dotted case, the
					;structure is what is
					;returned, not the content,
					;that is an S-expression
   '(((a b c)) . b)
   (cons '((a b c)) 'b))

  (lisp-unit:assert-equal
   '(a . b)
   (cons 'a 'b))

  (lisp-unit:assert-eq
   'a
   (car (cons 'a 'b)))

  (lisp-unit:assert-eq			;in this case we can use the
					;eq predicate because the cons
					;application return an
					;S-expression, not a cons
					;structure like when we handle
					;a stardard list (not a
					;'dotted' cons, which is not a
					;list)
   'b
   (cdr (cons 'a 'b)))

  (lisp-unit:assert-equal
   '(a b)
   (cons 'a (car '((b) c d))))

  (lisp-unit:assert-equal
   '(a c d)
   (cons 'a (cdr '((b) c d))))
  )

(lisp-unit:define-test null-questions-chapter-one
  (lisp-unit:assert-true
   (null (quote ())) )

  (lisp-unit:assert-false
   (null '(a b c)))

  (lisp-unit:assert-false
   (null 'spaghetti))

  (lisp-unit:assert-true
   (atomp 'Harry))

  (lisp-unit:assert-false
   (atomp '(Harry had a heap of apples)))

  (lisp-unit:assert-true
   (atomp (car '(Harry had a heap of apples))))

  (lisp-unit:assert-false
   (atomp (cdr '(Harry had a heap of apples))))

  (lisp-unit:assert-false
   (atomp (cdr '(Harry))))

  (lisp-unit:assert-true
   (atomp (car (cdr '(swing low sweet cherry oat)))))

  (lisp-unit:assert-false
   (atomp (car (cdr '(swing (low sweet) cherry oat)))))

  (lisp-unit:assert-eq
   'Harry
   'Harry)

  (lisp-unit:assert-false
   (eq 'margarine
       'butter))

  (lisp-unit:assert-false
   (eq () '(strawberry)))

  (lisp-unit:assert-false
   (eq 6
       7))

  (lisp-unit:assert-eq
   'Mary
   (car '(Mary had a little lamb chop)))

  (lisp-unit:assert-false
   (eq (cdr '(soured milk))
       'milk))

  (lisp-unit:assert-eq
   (car '(beans beans we need jelly beans))
   (car (cdr '(beans beans we need jelly beans))))
  )
