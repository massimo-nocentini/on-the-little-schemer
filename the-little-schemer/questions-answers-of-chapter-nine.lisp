(lisp-unit:define-test questions-of-chapter-nine

  (lisp-unit:assert-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
  (lisp-unit:assert-false (looking 'caviar '(6 2 grits caviar 5 7 3)))
)
