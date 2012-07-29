(in-package :com.github.massimo-nocentini.the-little-lisper)

;; looking at the tests for rember-f, now we've four functions which
;; do almost the same thing, with REMBER-F can simulate all the
;; others...
(defun rember-f (test? a l)
  "Assume TEST? a function that encapsulate the equality check to be
  applied to each element of L. This function perform the computation
  and return a list which is like l without the first atom #'eq to A
  if any."

  (cond
    ((null l) '())
    ((funcall test? a (car l)) (cdr l))	;use FUNCALL when invoking a
					;function argument or a
					;function that hasn't been
					;DEFUNed
    (t (cons (car l) (rember-f test? a (cdr l))))))

;; ...hence let's generate all the other versions with rember-gen
(defun rember-gen (test?)
  "This function, given a function that return true when the two
  arguments are equals, build (generate) a new function that is
  similar to some variant of REMBER"

  (lambda (a l)
    (cond
      ((null l) '())
      ((funcall test? a (car l)) (cdr l))
      (t (cons (car l)
	       (funcall (rember-gen test?) a (cdr l)))))))

(defun insert-gen (seq-f eq-f)
  "Assume SEQ a function of three parameter which return a cons
  structure to be returned as result of the generated function."

  (lambda (new old l)
    (cond
      ((null l) '())
      ((funcall eq-f old (car l))	;to use the ideas of
					;REMBER-GEN, which consume a
					;function to perform the
					;equality test, here we replay
					;that technique.
       (funcall seq-f new old (cdr l)))
      (t (cons (car l) (funcall (insert-gen seq-f eq-f)
				new old (cdr l)))))))



(define-test lambda-the-ultimate-from-little-lisper

  (assert-equal '(6 2 3)
		(rember-f (function =) 5 (quote (6 2 5 3))))
  (assert-equal '(beans are good)
		(rember-f (function eq) 'jelly '(jelly beans are good)))
  (assert-equal '(lemonade and (cake))
		(rember-f (function equal)
			  '(pop corn)
			  '(lemonade (pop corn) and (cake))))

  ;; Did we need to give a name (by defining rember-eq-generated for
  ;; example) to (rember-gen #'eq)? No, we could just write the
  ;; following:
  (assert-equal '(salad is good)
		(funcall (rember-gen #'eq) 'tuna '(tuna salad is good)))

  (assert-equal '(salad new-atom old-atom something else)
		(insert-gen (lambda (new old rest)
			      (cons new (cons old rest)))
			    #'eq))

  
  
  )
