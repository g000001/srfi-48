;;;; srfi-48.asd

(cl:in-package :asdf)

(defsystem :srfi-48
  :serial t
  :depends-on (:srfi-1 :srfi-5 :srfi-23)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-48")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-48))))
  (load-system :srfi-48)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-48-internal :srfi-48))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
