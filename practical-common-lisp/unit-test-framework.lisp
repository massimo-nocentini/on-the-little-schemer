(defun test-+-first-version ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))

(defun report-result-first-version (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+-second-version ()
  (report-result-first-version (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result-first-version (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result-first-version (= (+ -1 -2) -3) '(= (+ -1 -2) -3)))

;; we'd like to be able to say something like this (check (= (+ 1 2)
;; 3), have it mean the following (report-result (= (+ 1 2) 3) '(= (+
;; 1 2) 3))
(defmacro check-first-version (form)
  `(report-result-first-version ,form ',form))

(defun my-macro-expansion-tests ()
  (check-first-version (= (+ 1 2) 3)))

(defun test-+-third-version ()
  (check-first-version (= (+ 1 2) 3))
  (check-first-version (= (+ 1 2 3) 6))
  (check-first-version (= (+ -1 -2) -3)))

(defmacro check-second-version (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result-first-version ,f ',f))))

(defun test-+-fourth-version ()
  (check-second-version
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))

(defun report-result-second-version (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;; here I've copied the function from the file relative to the chapter
;; eight. When I'll able to use the package system I'll fix it
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check-third-version (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result-second-version ,f ',f))))

(deftest test-+-fiveth-version ()
  (check-third-version
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))

(deftest test-*-first-version ()
  (check-third-version
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic-first-version ()
  (combine-results
    (test-+-fiveth-version)
    (test-*-first-version)))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-math ()
  (test-arithmetic-first-version))