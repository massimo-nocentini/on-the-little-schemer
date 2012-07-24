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
     (unlimited-arguments-checker (and-or-arguments-getter l)))
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

(defun unlimited-arguments-checker (args)
  (cond
    ((null args) t)			;all the arguments are
					;well-formed logic expressions
    (t (and (lexp? (car args))		;make mutual recursion with
					;lexp? followed by natural
					;recursion on the remaining
					;arguments.
	    (unlimited-arguments-checker (cdr args))))))

(defun prefix-operator-getter (l)
  (car l))

(defun and-or-arguments-getter (l)
  (cdr l))
