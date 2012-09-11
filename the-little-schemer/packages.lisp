
(in-package :common-lisp-user)

(defpackage :com.github.massimo-nocentini.the-little-schemer
  (:use :common-lisp :lisp-unit))

;; the following package is used for the content and exercise of the
;; chapter "Lambda the Ultimate" from the book "The Little Lisper"
;; which is quite different respect the chapter with the same name in
;; the following book "The Little Schemer".
(defpackage :com.github.massimo-nocentini.the-little-lisper
  (:use :common-lisp :lisp-unit))