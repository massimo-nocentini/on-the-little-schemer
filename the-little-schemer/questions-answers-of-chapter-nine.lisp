(lisp-unit:define-test questions-of-chapter-nine

  (lisp-unit:assert-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
  (lisp-unit:assert-false (looking 'caviar '(6 2 grits caviar 5 7 3)))
  (lisp-unit:assert-equal '(a (b c))
			  (shift-pair '((a b) c)))
  (lisp-unit:assert-equal '(a (b (c d)))
			  (shift-pair '((a b) (c d))))
  (lisp-unit:assert-equal '((a (b h)) (c ((e f) g)))
			  (shift-pair '(((a (b h)) c) ((e f) g))))

  (lisp-unit:assert-eq 7 (pair-weight '((a b) c)))
  (lisp-unit:assert-eq 5 (pair-weight '(a (b c))))

  (lisp-unit:assert-equal '(a (a b))
		       (pair-shuffle '(a (a b))))
  (lisp-unit:assert-equal '(a b)
		       (pair-shuffle '(a b)))
  (lisp-unit:assert-equal '(c (a b))
		       (pair-shuffle '((a b) c)))

  (lisp-unit:assert-equal '((255 766 383 1150 575 1726 863 2590 1295
  3886 1943 5830 2915 8746 4373 13120 6560 3280 1640 820 410 205 616
  308 154 77 232 116 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8
  4 2 1) . 48)
			  (collatz (1- (expt 2 8)) (lambda (times numbers) (cons numbers times))))

  (lisp-unit:assert-equal 2
			  (ackermann 1 0
				     (lambda (times
				     pairs) (print (cons pairs
				     times)))))

  (lisp-unit:assert-equal 3 
			  (ackermann 1 1
				     (lambda (times
				     pairs) (print (cons pairs
				     times)))))
  (lisp-unit:assert-equal 7
			  (ackermann 2 2
				       (lambda (times
				     pairs) (print (cons pairs
				     times)))))

  (lisp-unit:assert-equal 9
			  (ackermann 2 3
				     (lambda (times
				     pairs) (print (cons pairs
				     times)))))

  (lisp-unit:assert-equal 61
			  (ackermann 3 3 (lambda (times
			  pairs) (print (cons pairs times)))))

)
