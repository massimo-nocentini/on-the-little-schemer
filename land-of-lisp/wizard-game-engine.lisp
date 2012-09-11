(defparameter *nodes* 
  '(
    (living-room (you are in the living-room.
		  a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
	     there is a well in front of you.))
    (attic (you are in the attic.
	    there is a giant welding torch in the corner.))))

(defparameter *edges* 
  '(
    (living-room (garden west door) (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defparameter *objects* 
  '(whiskey bucket frog chain))

(defparameter *object-locations* 
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun describe-location (location nodes)
  (car (cdr (assoc location nodes))))

(defun describe-path (edge)
  ;; nice example of quasiquoting feature: mixing code to be evaluated
  ;; in data code, using flip-flop of ` and ,
  `(there is a ,(car (cdr (cdr edge))) going ,(car (cdr edge)) from here.))

(defun describe-paths (location edges)
"This function encapsulate the rule for build a list with the
description of each path, passing the necessary dependency of the
collect-paths-description-into-one-list function with the correct
value."
  (apply
   (function append)
   (collect-paths-description-into-one-list 
    (get-outgoing-paths location edges))))

(defun collect-paths-description-into-one-list (paths)
"This function encapsulate the rules of describing a list of path
objects given as parameter. It doesn't know how to retrieve the path
objects, it knows only how to assemble them for obtain a description
for each path. In this way this function is decoupled from the
get-outgoing-paths function."
  (mapcar (function describe-path) paths))

(defun get-outgoing-paths (location edges)
"This function encapsulate the concept of how to retrieve the paths
exiting from the given location."
  (cdr (assoc location edges)))

(defun objects-at (loc objs obj-locs)
  (labels (
	   ;; here we define a predicate, using closure on variable
	   ;; loc this is very helpful because it provide an
	   ;; abstraction for knowing if an object is in the hardcoded
	   ;; loc location.
	   (at-loc-p (obj)
	     (eq (car (cdr (assoc obj obj-locs))) loc))
	   )
    (remove-if-not (function at-loc-p) objs)))

(defun describe-objects (loc objs obj-loc)
  (labels (
	   (describe-obj (obj)
	     `(you see a ,obj on the floor.)
	     )
	   )
    ;; here we use the apply function because it allow us to use the
    ;; items contained in a list (in this context the list is returned
    ;; by mapcar) as arguments to another function (in this context
    ;; the append function)
    (apply 
     (function append)
     (mapcar (function describe-obj) (objects-at loc objs obj-loc)))
    ))

(defun look ()
  (append
   (describe-location *location* *nodes*)
   (describe-paths *location* *edges*)
   (describe-objects *location* *objects* *object-locations*))
  )

(defun walk (direction)
  (let (
	(next (find direction (cdr (assoc *location* *edges*)) 
		    :key (function cadr)))
	)
    (if next 
	(progn (setf *location* (car next))
	       (look))
	(quote (you cannot go that way.))
	)
    )
  )

(defun pickup (object)
  (cond (
	 (member object (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t (quote (you cannot get that.)))
	)
  )

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-read ()
  ;; using read-from-string function we can build S-expression, which are
  ;; pure Lisp computation object parsing a string. Hence the variable cmd
  ;; hosts a pure Lisp S-expression
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     ;; using cons instead of list is wrong because x is an
	     ;; object not a list, so using a cons form produce a pair
	     ;; of object and not a list
	     (list 'quote x)))
      ;; now we can quote-it all the argument given by the user and
      ;; build a new cons with the command entered in the first
      ;; position, getting it from cmd variable directly
      (cons (car cmd) (mapcar (function quote-it) (cdr cmd)))
      )
    )
  )

(defun game-eval (sexp)
  ;; checking that the function of the form encapsulated by sexp
  ;; argument is a command that is in our game scope. Anything else
  ;; will be banned and returned to sender.
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)
      )
  )

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl)
      )
    )
  )