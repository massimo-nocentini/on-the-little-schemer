(in-package :com.github.massimo-nocentini.the-little-schemer)
  
(defun lexp? (l)
  (cond
    ((null l) t)			;the empty clausole is valid
					;however always false!
    ((atomp l) (not (numberp l)))	;predicate that characterize
					;the set of logic expressions
    ((or (eq 'and (prefix-operator-getter l)) 
	 (eq 'or (prefix-operator-getter l)))
     (unlimited-arguments-checker	;for AND and OR op we allow
					;illimited logic
					;sub-expression arguments
      (and-or-arguments-getter l)
      #'lexp?))				;here we can just pass the
					;current LEXP? function as
					;argument because the argument
					;to this function will be an
					;argument to check. We've not
					;to close under any other
					;object, for a comparison see
					;the function COVERED?
    ((and (eq 2 (length l))		;for the NOT operator we
					;require exactly one
					;sub-logic-exp, more than NOT
					;symbol itself.
	  (eq 'not (prefix-operator-getter l)))
     (lexp? (not-arguments-getter l)))	;just recur with the single
					;sub expression to check
    (t nil)))				;all other lists are not
					;logic expressions

(defun not-arguments-getter (l)
  (car (cdr l)))			;explicitly return only one
					;sexp (which can be an atom or
					;a list of course)

(defun unlimited-arguments-checker
    (args
     mutual-recursion
     &key (composer (lambda (fst snd)
		      (and fst snd)))
     (base-value t))			;the base value is T, the
					;neutral element of AND
					;function (see the default
					;lambda for COMPOSER argument)
  (cond
    ((null args) base-value)		;all the arguments are
					;well-formed logic expressions
    (t (funcall composer
		(funcall mutual-recursion (car args)) ;make mutual
						      ;recursion with
						      ;lexp?  followed
						      ;by natural
						      ;recursion on
						      ;the remaining
						      ;arguments.
		(unlimited-arguments-checker
		 (cdr args)  mutual-recursion
		 :composer composer :base-value base-value)))))

(defun prefix-operator-getter (l)
  (car l))

(defun and-or-arguments-getter (l)
  (cdr l))

(defun covered? (lexp los)
  "Assume that lexp is a valid logic expression"
  (cond
    ((null lexp) t)
    ((atomp lexp) (memberp lexp los))
    ((or (eq 'and (prefix-operator-getter lexp)) ;for AND and OR op we
					      ;allow illimited logic
					      ;expression arguments
	 (eq 'or (prefix-operator-getter lexp)))
     (unlimited-arguments-checker
      (and-or-arguments-getter lexp)
      #'(lambda (argument-lexp)		;here we have to pass this
					;lambda because we care about
					;the LOS argument because is
					;necessary for the application
					;of covered? to the argument
					;of the lambda (otherwise we
					;could have passed LOS to the
					;function
					;UNLIMITED-ARGUMENTS-CHECKER,
					;but that new argument is
					;relative to only the function
					;covered?, for the function
					;lexp? is meaningless to pass
					;it: this help to keep the
					;signature of
					;UNLIMITED-ARGUMENTS-CHECKER
					;clean and short.
	  (covered? argument-lexp los))))
    (t (covered?		       ;since we assume that LEXP is a
				       ;valid logic expression, if the
				       ;preceeding cond-line fail,
				       ;here the operator have to be
				       ;NOT
	(not-arguments-getter lexp) los)))) ;just recur with the
					    ;single sub expression
					    ;tocheck
 
    
(defun lookup (a al)
  "Assume AL an association list."
  (cond
    ((null al) nil)
    ((eq a (car (car al))) (car (cdr (car al))))
    (t (lookup a (cdr al)))))

(defun extract-variables (al)
  "Assume AL an association list."
  (cond
    ((null al) '())
    (t (cons (car (car al))
	     (extract-variables (cdr al))))))

(defun meaning-of-lexp (lexp al)
  "Assume LEXP a logic expression and AL an association list (the
  state)"
  (cond
    ((not (lexp? lexp)) 'not-a-logic-expression)
    ((null lexp) nil)
    ((covered? lexp (extract-variables al))
     (cond
       ((atomp lexp) (lookup lexp al))
       ((eq 'and (prefix-operator-getter lexp))
	(unlimited-arguments-checker
	 (and-or-arguments-getter lexp)
	 #'(lambda (argument-lexp)
	     (meaning-of-lexp argument-lexp al))))
       ((eq 'or (prefix-operator-getter lexp))
	(unlimited-arguments-checker
	 (and-or-arguments-getter lexp)
	 #'(lambda (argument-lexp)
	     (meaning-of-lexp argument-lexp al))
	 :composer (lambda (fst snd)
		      (or fst snd))	;here we cannot pass #'or
					;because OR is a macro, not a
					;function.
	 :base-value nil))		;NIL is the neutral element
					;for OR function
       (t (not (meaning-of-lexp (not-arguments-getter lexp) al)))))
    (t 'not-covered)))