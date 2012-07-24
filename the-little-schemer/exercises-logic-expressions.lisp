(in-package :com.github.massimo-nocentini.the-little-schemer)
  
(defun lexp? (l)
  (cond
    ((null l) t)			;the empty clausole is valid
					;however always false!
    ((atomp l) (not (numberp l)))	;predicate that characterize
					;the set of logic expressions
    ((or (eq 'and (prefix-operator-getter l)) ;for AND and OR op we
					      ;allow illimited logic
					      ;expression arguments
	 (eq 'or (prefix-operator-getter l)))
     (unlimited-arguments-checker
      (and-or-arguments-getter l)
      #'lexp?))
    ((and (eq 2 (length l))		;for the NOT operator we
					;require exactly one
					;sub-logic-exp
	  (eq 'not (prefix-operator-getter l)))
     (lexp? (not-arguments-getter l)))	;just recur with the single
					;sub expression to check
    (t nil)))				;all other lists are not
					;logic expressions

(defun not-arguments-getter (l)
  (car (cdr l)))			;explicitly return only one
					;sexp (which can be an atom or
					;a list of course)

(defun unlimited-arguments-checker (args mutual-recursion)
  (cond
    ((null args) t)			;all the arguments are
					;well-formed logic expressions
    (t (and (funcall mutual-recursion
		     (car args))	;make mutual recursion with
					;lexp? followed by natural
					;recursion on the remaining
					;arguments.
	    (unlimited-arguments-checker (cdr args)
					 mutual-recursion)))))

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
 
    
  