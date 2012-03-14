(lisp-unit:define-test questions-of-chapter-six  

  (lisp-unit:assert-eq 'a (quote a) )
  (lisp-unit:assert-eq 'o+ (quote o+))
  (lisp-unit:assert-eq 'x (quote x))

  (lisp-unit:assert-true (numberedp 1))
  (lisp-unit:assert-true (numberedp '(3 o+ (4 expt 5))))
  (lisp-unit:assert-false (numberedp '(2 x sausage)))

  (lisp-unit:assert-eq 13 (tls-value '13))
  (lisp-unit:assert-eq 4 (tls-value '(1 + 3)))
  (lisp-unit:assert-eq 82 (tls-value '(1 + (3 ^ 4))))
)