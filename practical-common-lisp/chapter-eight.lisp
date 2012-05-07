;; suppose you start with the idea that you want to be able to write
;; the following:
(do-primes (p 0 19)
  (format t "~d " p))

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number
       fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; without the do-primes macro, you could write such a loop with DO
;; like this:
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

;; second version of do-primes macro written using a
;; macro-that-write-a-macro style
(defmacro do-primes-macro-writing-macro ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name)) ,@body)) )

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))