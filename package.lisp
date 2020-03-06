;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-48"
  (:use)
  (:export format))


(defpackage "https://github.com/g000001/srfi-48#internals"
  (:use cl fiveam)
  (:shadow loop map member assoc)
  (:shadowing-import-from "https://github.com/g000001/srfi-5" let)
  (:shadowing-import-from "https://github.com/g000001/srfi-23" error)
  (:shadowing-import-from "https://github.com/g000001/srfi-48" format))


;;; *EOF*
