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

(defun value-0 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. This is the basic version from
which we start our refactoring process."
  (cond
    ((numberp aexp) aexp)
    ((eq '+ (car aexp)) (+ (value (cadr aexp)) ;code repeated
			   (value (caddr aexp))))
    ((eq '* (car aexp)) (* (value (cadr aexp))
			   (value (caddr aexp))))))

(defun value-1 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we abstract
away the association with the operators #'+ and #'*."
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

(defun value-2 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we factor the
abstracted code, passing it as an argument, hence the major structure
of the code present in the previous version, is wrapped in a lambda
which is immediately executed."
  ((lambda (apply-with-operator)
     (cond
       ((numberp aexp) aexp)
       ;; the following are two cond-lines with a similar structure
       ((eq '+ (car aexp)) (funcall apply-with-operator #'+))
       ((eq '* (car aexp)) (funcall apply-with-operator #'*))))
   (lambda (op-fun) (funcall op-fun
			     (value (cadr aexp))
			     (value (caddr aexp))))))

(defun value-3 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we try to
abstract and later factor, the common structure of the cond-lines
commented in the previous version."
  ((lambda (apply-with-operator recfun)
     (cond
       ((numberp aexp) aexp)
       ;; with this refactoring step we introduce too much complexity
       ;; to the abstracted lambda (4 arguments are passed in!).
       (t (or (funcall recfun '+ (car aexp) #'+ apply-with-operator)
	      (funcall recfun '* (car aexp) #'* apply-with-operator)))))
   (lambda (op-fun) (funcall op-fun
			     (value (cadr aexp))
			     (value (caddr aexp))))
   (lambda (op-symbol operator operator-f apply-with-operator)
	   (when (eq op-symbol operator)
	     (funcall apply-with-operator operator-f)))))

(defun value-4 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we inline the
lambda associated to the symbol apply-with-operator (the first we
extracted). For this refactoring we just copy the lambda definition an
put it in the first position of a sexp, replacing the old (funcall
apply-with-operator..."
  ((lambda (recfun)
     (cond
       ((numberp aexp) aexp)
       (t (or (funcall recfun '+ (car aexp) #'+ )
	      (funcall recfun '* (car aexp) #'* )))))
   (lambda (op-symbol operator operator-f)
     ;; this lambda seems to do too many things: first it checks if
     ;; the PASSED ARGUMENTS about the operator allow to (second)
     ;; invoke the corresponding function. 
     (when (eq op-symbol operator)	;here we use only the passed
					;argument for computing the
					;comparison. Unless we want
					;flexibility about the
					;equality comparer, this
					;operation can be performed
					;outside this lambda, passing
					;only the operator-f function,
					;which is the only true
					;dependency this function
					;needs.
       ((lambda (op-fun)
	  (funcall op-fun
		   (value (cadr aexp))
		   (value (caddr aexp)))) operator-f)))))

(defun value-5 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we abstract
away the two or-branches in order to inline the OR completely. In this
refactoring we define a lambda which doesn't abstract its inner
association as done in the previous versions, but we introduce a cond
expression in order to cover both cases which the OR expr covers. Pay
attention to this: this step should be applied with some little
adjustments on version 2."
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

(defun value-6 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. In this version we factor the
abstracted lambda about the OR expr, making a wrapping lambda, put it
in the function position and execute it passing the abstracted lambda
about the OR behavior."
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

(defun value-7 (aexp)
  "Assume AEXP an arithmetic expression and the only operator that can
appear in it are the + and * operator. This refactoring step seems
like refactoring toward polymorphims: the two cond-lines about the
operator have just moved in other lambda (like send a message to
another object instead to perform the computation...)".
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
appear in it are the + and * operator. The final version obtained
inlining the lambda about the application of the operator function
corresponding to the operator symbol."
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

;; the following functions SUBSET? and INTERSECT? have the same
;; structure hence we should abstract them into a function SET-F which
;; can generate both of them.
(defun subset?-duplicate-code (set1 set2)
  (cond
    ((null set1) t)
    (t (and (member (car set1) set2)
	    (subset? (cdr set1) set2)))))

(defun intersect?-duplicate-code (set1 set2)
  (cond
    ((null set1) nil)
    (t (or (member (car set1) set2)
	   (intersect? (cdr set1) set2)))))

;; in order to abstract the previous functions I've performed the
;; following steps:
;;    1. make a copy of one of them (I choose INTERSECT?)
;;    2. insert a new line on top with the DEFUN form
;;    3. changed the old (DEFUN INTERSECT? with (LAMBDA
;; all the remaining code of the function remains the same except
;; the recursive invocation because we have to pass again the
;; LOGICAL and CONST arguments and then invoke the newly built
;; function.
(defun set-f-before-*prime-logical (logical const)
  (lambda (set1 set2)
    (cond
      ((null set1) const)
      (t (funcall logical
		  (member (car set1) set2)
		  (funcall (set-f logical const)
			   (cdr set1)
			   set2))))))

;; now what we've abstracted almost work because AND and OR are
;; macros, not functions, hence we have more work to get done, namely
;; create a first order AND and OR functions, in order to pass them as
;; arguments.
;; (defun intersect? (set1 set2)
;;   (funcall (set-f #'and nil) set1 set2))

;; the following functions are a shy implementations because before
;; their computations start, both arguments have to be evaluated,
;; which aren't what AND and OR truly do: they do now always ask the
;; second question.
(defun and-first-order-incorrect (x y)
  (and x y))
(defun or-first-order-incorrect (x y)
  (or x y))

;; in order to have the right behavior we have to define an ad-hoc
;; version of AND and OR, which associate them with the functions
;; INTERSECT? and SUBSET?. Here the first argument is given, the other
;; two are the two set which we have to use with the recursion.  Pay
;; attention: when we're writing these functions, both SUBSET? both
;; INTERSECT? do not exists!
(defun and-prime (x set1 set2)
  (and x (subset? (cdr set1) set2)))

;; PAY ATTENTION: after we finished to type this two definitions, the
;; functions SUBSET? and INTERSECT? no longer exist. Compiling the
;; source, LISP tell us a style warning about their nonexistence but
;; doesn't halt the compilation (try to change the symbol INTERSECT
;; with INTERSECT-NON-EXISTENT to reproduce the warning).
(defun or-prime (x set1 set2)
  (or x (intersect? (cdr set1) set2)))

;; now we can define the nonexistent SUBSET? and INTERSECT? using the
;; *-prime functions defined just for them...awfully recursive!
(defun intersect? (set1 set2)
  (funcall (set-f #'or-prime nil) set1 set2))

(defun subset? (set1 set2)
  (funcall (set-f #'and-prime t) set1 set2))

;; now we've to adjust the last line where we call the passed LOGICAL
;; function, giving to it the first MEMBER check and the two sets.
(defun set-f (logical const)
  (lambda (set1 set2)
    (cond
      ((null set1) const)
      (t (funcall logical
		  (member (car set1) set2) set1 set2)))))

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

  (assert-false (intersect? '(3) '(2 4)))
  (assert-true (intersect? '(2) '(2 4)))

  (assert-false (subset? '(3) '(2 4)))
  (assert-true (subset? '(2) '(2 4)))    

  )

