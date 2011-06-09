;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-48
  (:use)
  (:export :format))

(defpackage :srfi-48-internal
  (:use :cl :fiveam)
  (:shadow :loop :map :member :assoc)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-48 :format))

