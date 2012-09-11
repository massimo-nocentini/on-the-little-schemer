(in-package :com.github.massimo-nocentini.the-little-schemer)

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

  ;; (lisp-unit:assert-eq 0 (funcall (make-length-zero-lambda ) '()))

  ;; (lisp-unit:assert-eq 0 (funcall (make-length-at-most-one-lambda) '()))
  ;; (lisp-unit:assert-eq 1 (funcall (make-length-at-most-one-lambda)
  ;; '(j)))

  ;; (lisp-unit:assert-eq 0 (funcall
  ;; 			  (make-length-at-most-one-with-mklength ) '()))
  ;; (lisp-unit:assert-eq 1 (funcall
  ;; 			  (make-length-at-most-one-with-mklength )
  ;; 			  '(apples)))
  

  ;; (lisp-unit:assert-eq 0 (funcall
  ;; 			  (make-length-at-most-two-lambda) '()))
  ;; (lisp-unit:assert-eq 1 (funcall
  ;; 			  (make-length-at-most-two-lambda) '(j)))
  ;; (lisp-unit:assert-eq 2 (funcall
  ;; 			  (make-length-at-most-two-lambda) '(j (a))))

  ;; (lisp-unit:assert-eq 0 (funcall
  ;; 			  (make-length-at-most-three-lambda) '()))
  ;; (lisp-unit:assert-eq 1 (funcall
  ;; 			  (make-length-at-most-three-lambda) '(j)))
  ;; (lisp-unit:assert-eq 2 (funcall
  ;; 			  (make-length-at-most-three-lambda) '(j (a))))
  ;; (lisp-unit:assert-eq 3 (funcall
  ;; 			  (make-length-at-most-three-lambda) '(j (a) k)))

  ;; the following tests doesn't halt because of the invocation of
  ;; make-lenght-lambda with argument itself  
  ;; (lisp-unit:assert-eq 0 (funcall
  ;; 			  (make-length) '()))
  ;; (lisp-unit:assert-eq 1 (funcall
  ;; 			  (make-length) '(j)))
  ;; (lisp-unit:assert-eq 2 (funcall
  ;; 			  (make-length) '(j (a))))
  ;; (lisp-unit:assert-eq 10 (funcall
  ;; 			  (make-length) '(0 1 2 3 4 5 6 7 8 9)))

  ;; the following tests all pass because the invocation of the lambda
  ;; make-length-lambda with argument himself is done only when
  ;; needed, adding only one layer to the recursion tower.
  (lisp-unit:assert-eq 0 (funcall
  			  (make-length-but-not-seems-like-length)
  			  '()))
  (lisp-unit:assert-eq 1 (funcall
  			  (make-length-but-not-seems-like-length)
  			  '(j)))
  (lisp-unit:assert-eq 2 (funcall
  			  (make-length-but-not-seems-like-length)
  			  '(j (a))))
  (lisp-unit:assert-eq 10 (funcall
			   (make-length-but-not-seems-like-length) '(0
			   1 2 3 4 5 6 7 8 9)))

  (lisp-unit:assert-eq 0 (funcall
  			  (make-length-toward-soundness)
  			  '()))
  (lisp-unit:assert-eq 1 (funcall
  			  (make-length-toward-soundness)
  			  '(j)))
  (lisp-unit:assert-eq 2 (funcall
  			  (make-length-toward-soundness)
  			  '(j (a))))
  (lisp-unit:assert-eq 10 (funcall
			   (make-length-toward-soundness) '(0 1 2 3 4
			   5 6 7 8 9)))

  (lisp-unit:assert-eq 0 (funcall
  			  (make-length-toward-y-combinator)
  			  '()))
  (lisp-unit:assert-eq 1 (funcall
  			  (make-length-toward-y-combinator)
  			  '(j)))
  (lisp-unit:assert-eq 2 (funcall
  			  (make-length-toward-y-combinator)
  			  '(j (a))))
  (lisp-unit:assert-eq 10 (funcall
			   (make-length-toward-y-combinator) '(0 1 2 3
			   4 5 6 7 8 9)))

  (lisp-unit:assert-eq 0 (length-y-combinator-powered '()))
  (lisp-unit:assert-eq 1 (length-y-combinator-powered '(j)))
  (lisp-unit:assert-eq 2 (length-y-combinator-powered '(j (a))))
  (lisp-unit:assert-eq 10 (length-y-combinator-powered '(0 1 2 3 4 5 6
							 7 8 9)))

  
  
  
  (lisp-unit:assert-eq 0 (eternity-length '()))
  (lisp-unit:assert-eq 1 (eternity-length '(j)))
  (lisp-unit:assert-eq 2 (eternity-length '(j k)))    

)
