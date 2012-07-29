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

(defun insert-gen-eq (seq-f)
  "Assume SEQ a function of three parameter which return a cons
  structure to be returned as result of the generated function."

  (lambda (new old l)
    (cond
      ((null l) '())
      ((eq old (car l))			;here we fix the function that
					;check the equality, using
					;#'eq. This isn't flexible
					;like REMBER-GEN shows, hence
					;in the following INSERT-GEN
					;we try to abstract the
					;association with #'eq.
       (funcall seq-f new old (cdr l)))
      (t (cons (car l) (funcall (insert-gen-eq seq-f)
				new old (cdr l)))))))

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


(defun REMBER-F-generate-something-that-INSERT-GEN-can-generate? (a l)
  (eq (funcall (rember-gen #'equal) a l)
      (funcall (insert-gen (lambda (new old rest)
			     rest)
			   #'equal)
	       nil a l)))

(defun value-with-repetition (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  (cond
    ((numberp aexp) aexp)
    ((eq '+ (car aexp)) (+ (value (cadr aexp))
			   (value (caddr aexp))))
    ((eq '* (car aexp)) (* (value (cadr aexp))
			   (value (caddr aexp))))))

(defun value-with-encapsulated-common-behavior (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  (cond
    ((numberp aexp) aexp)
    ((eq '+ (car aexp)) ((lambda (op-fun)
			   (funcall op-fun
				    (value (cadr aexp))
				    (value (caddr aexp)))) #'+))
    ((eq '* (car aexp)) ((lambda (op-fun)
			   (funcall op-fun
				    (value (cadr aexp))
				    (value (caddr aexp)))) #'*))))

(defun value-with-abstracted-common-behavior (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (apply-with-operator)
     (cond
       ((numberp aexp) aexp)
       ((eq '+ (car aexp)) (funcall apply-with-operator #'+))
       ((eq '* (car aexp)) (funcall apply-with-operator #'*))))
   (lambda (op-fun) (funcall op-fun
			     (value (cadr aexp))
			     (value (caddr aexp))))))

(defun value-with-simplified-cond-expr (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (apply-with-operator recfun)
     (cond
       ((numberp aexp) aexp)
       (t (or (funcall recfun '+ (car aexp) #'+ apply-with-operator)
	      (funcall recfun '* (car aexp) #'* apply-with-operator)))))
   (lambda (op-fun) (funcall op-fun
			     (value (cadr aexp))
			     (value (caddr aexp))))
   (lambda (op-symbol operator operator-f apply-with-operator)
	   (when (eq op-symbol operator)
	     (funcall apply-with-operator operator-f)))))

(defun value-with-simplified-inner-lambdas (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (recfun)
     (cond
       ((numberp aexp) aexp)
       (t (or (funcall recfun '+ (car aexp) #'+ )
	      (funcall recfun '* (car aexp) #'* )))))
   (lambda (op-symbol operator operator-f)
     (when (eq op-symbol operator)
       ((lambda (op-fun) (funcall op-fun
				  (value (cadr aexp))
				  (value (caddr aexp)))) operator-f)))))

(defun value-with-prepared-op-function-builder (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (recfun)
     (cond
       ((numberp aexp) aexp)
       (t (or (funcall recfun
		       ((lambda (operator)
			  (cond
			    ((eq '+ operator) #'+)
			    ((eq '* operator) #'*))) (car aexp)))
	      (funcall recfun
 		       ((lambda (operator)
			  (cond
			    ((eq '+ operator) #'+)
			    ((eq '* operator) #'*))) (car aexp)))))))
   (lambda (operator-f)
     (funcall operator-f
	      (value (cadr aexp))
	      (value (caddr aexp))))))

(defun value-with-doubled-or-branches (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (recfun op-to-op-f)
     (cond
       ((numberp aexp) aexp)
       (t (or (funcall recfun (funcall op-to-op-f (car aexp)))
	      (funcall recfun (funcall op-to-op-f (car aexp)))))))
   (lambda (operator-f)
     (funcall operator-f
	      (value (cadr aexp))
	      (value (caddr aexp))))
   (lambda (operator)
     (cond
       ((eq '+ operator) #'+)
       ((eq '* operator) #'*)))))

(defun value-prepared-for-reclambda-inlining (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (recfun op-to-op-f)
     (cond
       ((numberp aexp) aexp)
       (t (funcall recfun (funcall op-to-op-f (car aexp))))))
   (lambda (operator-f)
     (funcall operator-f
	      (value (cadr aexp))
	      (value (caddr aexp))))
   (lambda (operator)
     (cond
       ((eq '+ operator) #'+)
       ((eq '* operator) #'*)))))

(defun value (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator."
  ((lambda (op-to-op-f)
     (cond
       ((numberp aexp) aexp)
       (t (funcall (funcall op-to-op-f (car aexp))
		   (value (cadr aexp))
		   (value (caddr aexp))))))
   (lambda (operator)
     (cond
       ((eq '+ operator) #'+)
       ((eq '* operator) #'*)))))


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
		(funcall (insert-gen
			  (lambda (new old rest) ;instead of defining
						 ;a function for setL,
						 ;we pass its
						 ;definition instead,
						 ;directly building an
						 ;(anonymous)
						 ;function. It is
						 ;better because you
						 ;do not need to
						 ;remember as many
						 ;names.
			    (cons new (cons old rest)))
			  #'eq)
			 'new-atom
			 'old-atom
			 '(salad old-atom something else)))

  (assert-true
   '(salad something else)
   (REMBER-F-generate-something-that-INSERT-GEN-can-generate?
    'old-atom '(salad old-atom something else)))

  (assert-eq 4 (value '(+ 1 3)))
  (assert-eq 16 (value '(+ 1 (* 3 5))))

  )

