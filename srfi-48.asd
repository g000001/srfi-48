;;;; srfi-48.asd

(cl:in-package :asdf)


(defsystem :srfi-48
  :version "20200307"
  :description "SRFI 48 for CL: Intermediate Format Strings"
  :long-description "SRFI 48 for CL: Intermediate Format Strings
https://srfi.schemers.org/srfi-48"
  :author "Kenneth A Dickey"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:srfi-1 :srfi-5 :srfi-23)
  :components ((:file "package")
               ;;(:file "utils")
               (:file "srfi-48")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-48))))
  (let ((name "https://github.com/g000001/srfi-48")
        (nickname :srfi-48))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-48))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-48#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-48)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
