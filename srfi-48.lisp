;;;; srfi-48.lisp

(cl:in-package :srfi-48-internal)

(def-suite srfi-48)

(in-suite srfi-48)

;; IMPLEMENTATION DEPENDENT options

(defconstant ascii-tab   (code-char  9))  ;; NB: assumes ASCII encoding
(defconstant dont-print nil)
(defun pretty-print (arg port)
  (write arg :stream port :pretty T))

(defun string-index (str c)
  (let ((len (length str)))
    (let loop ( (i 0) )
         (cond ((= i len) nil)
               ((eql c (char str i)) i)
               (:else (loop (+ i 1)))))))

(defun string-append (&rest strings)
  (declare (optimize (safety 0) (speed 3))
           (dynamic-extent strings))
  (let ((len 0)
        (pos 0))
    (declare (fixnum len pos))
    (dolist (s strings)
      (declare (simple-string s))
      (incf len (length s)))
    (let ((result (make-string len)))
      (declare (simple-string result))
      (dolist (s strings)
        (declare (simple-string s))
        (cl:loop :for c :across s
                 :do (setf (schar result pos) c) (incf pos)))
      result)))

(defun string-grow (str len char)
  (let ( (off (- len (length str))) )
    (if (plusp off)
        (string-append (make-string off :initial-element char) str)
        str)))

(defun compose-with-digits (digits pre-str frac-str exp-str)
  (let ((frac-len (length frac-str)))
    (cond
      ((< frac-len digits) ;; grow frac part, pad with zeros
       (string-append pre-str "."
                      frac-str (make-string (- digits frac-len)
                                            :initial-element #\0)
                      exp-str))
      ((= frac-len digits) ;; frac-part is exactly the right size
       (string-append pre-str "."
                      frac-str
                      exp-str))
      (:else ;; must round to shrink it
       (let* ( (first-part (subseq frac-str 0 digits))
               (last-part  (subseq frac-str digits frac-len))
               (temp-str
                (write-to-string
                 (round (read-from-string
                         (string-append first-part "." last-part)))))
               (dot-pos (string-index  temp-str #\.))
               (carry?
                (and (> dot-pos digits)
                     (> (round (read-from-string
                                (string-append "0." frac-str)))
                        0)))
               (new-frac
                (subseq temp-str 0 digits)))
         (string-append
          (if carry?
              (write-to-string (+ 1 (write-to-string pre-str)))
              pre-str)
          "."
          new-frac
          exp-str))))))

(defun format-fixed (number-or-string width digits) ; returns a string
  (cond
    ((stringp number-or-string)
     (string-grow number-or-string width #\space))
    ((numberp number-or-string)
     (let ((real (realpart number-or-string))
           (imag (imagpart number-or-string)))
       (cond
         ((not (zero? imag))
          (string-grow
           (string-append (format-fixed real 0 digits)
                          (if (minusp imag) "" "+")
                          (format-fixed imag 0 digits)
                          "i")
           width
           #\space) )
         (digits
          (let* ((num-str   (write-to-string (float real)))
                  (dot-index (string-index  num-str #\.))
                  (exp-index (string-index  num-str #\e))
                  (length    (length num-str))
                  (pre-string
                   (cond
                     (exp-index
                      (if dot-index
                          (subseq num-str 0 dot-index)
                          (subseq num-str 0 (+ exp-index 1))))
                     (dot-index
                      (subseq num-str 0 dot-index))
                     (:else num-str)))
                  (exp-string
                   (if exp-index (subseq num-str exp-index length) "") )
                  (frac-string
                   (if exp-index
                       (subseq num-str (+ dot-index 1) exp-index)
                       (subseq num-str (+ dot-index 1) length)) ) )
            (string-grow
             (if dot-index
                 (compose-with-digits digits
                                      pre-string
                                      frac-string
                                      exp-string)
                 (string-append pre-string exp-string))
             width
             #\space) ))
         (:else ;; no digits
          (string-grow (write-to-string real) width #\space))) ))
    (:else
     (error
      (format "FORMAT: ~F requires a number or a string, got ~s"
              number-or-string))) ))

(setf (documentation 'format 'function)
"(format [<port>] <format-string> [<arg>...]) -- <port> is T, nil or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
" )

(defun require-an-arg (args)
  (when (null args)
    (error "FORMAT: too few arguments" )))

(defun format-help (format-strg arglist port)
  (let ((length-of-format-string (length format-strg)))
    (labels
      ((anychar-dispatch (pos arglist last-was-newline)
         (if (>= pos length-of-format-string)
             arglist ; return unused args
             (let ((char (char format-strg pos)))
               (cond
                 ((eql char #\~)
                  (tilde-dispatch (+ pos 1) arglist last-was-newline))
                 (:else
                  (write-char char port)
                  (anychar-dispatch (+ pos 1) arglist nil))))))
       (has-newline? (whatever last-was-newline)
         (or (eql whatever #\newline)
             (and (string? whatever)
                  (let ( (len (length whatever)) )
                    (if (zero? len)
                        last-was-newline
                        (eql #\newline
                             (char whatever (- len 1))))))))
       (tilde-dispatch (pos arglist last-was-newline)
         (cond
           ((>= pos length-of-format-string)
            (write-char #\~ port) ; tilde at end of string is just output
            arglist ; return unused args
            )
           (:else
            (case (char-upcase (char format-strg pos))
              ((#\A)       ; Any -- for humans
                 (require-an-arg arglist)
                 (let ( (whatever (car arglist)) )
                   (princ whatever port)
                   (anychar-dispatch (+ pos 1)
                                     (cdr arglist)
                                     (has-newline? whatever
                                                   last-was-newline))))
              ((#\S)       ; Slashified -- for parsers
                 (require-an-arg arglist)
                 (let ( (whatever (car arglist)) )
                   (write whatever :stream port)
                   (anychar-dispatch (+ pos 1)
                                     (cdr arglist)
                                     (has-newline? whatever
                                                   last-was-newline))))
              ((#\W)
                 (require-an-arg arglist)
                 (let ( (whatever (car arglist)) )
                   (write whatever :stream port :circle T)  ;; srfi-38
                   (anychar-dispatch (+ pos 1)
                                     (cdr arglist)
                                     (has-newline? whatever
                                                   last-was-newline))))
              ((#\D)       ; Decimal
                 (require-an-arg arglist)
                 (princ (write-to-string (car arglist) :base 10) port)
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil))
              ((#\X)       ; HeXadecimal
                 (require-an-arg arglist)
                 (princ (write-to-string (car arglist) :base 16) port)
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil) )
              ((#\O)       ; Octal
                 (require-an-arg arglist)
                 (princ (write-to-string (car arglist) :base 8) port)
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil) )
              ((#\B)       ; Binary
                 (require-an-arg arglist)
                 (princ (write-to-string  (car arglist) :base 2) port)
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil) )
              ((#\C)       ; Character
                 (require-an-arg arglist)
                 (write-char (car arglist) port)
                 (anychar-dispatch (+ pos 1)
                                   (cdr arglist)
                                   (eql (car arglist) #\Newline)) )
              ((#\~)       ; Tilde
                 (write-char #\~ port)
                 (anychar-dispatch (+ pos 1) arglist nil))
              ((#\%)       ; Newline
                 (terpri port)
                 (anychar-dispatch (+ pos 1) arglist T))
              ((#\&)      ; Freshline
                 (if (not last-was-newline) ;; (unless last-was-newline ..
                     (terpri port))
                 (anychar-dispatch (+ pos 1) arglist T))
              ((#\_)       ; Space
                 (write-char #\space port)
                 (anychar-dispatch (+ pos 1) arglist nil) )
              ((#\T)
                 ;; Tab -- IMPLEMENTATION DEPENDENT ENCODING
                 (write-char ascii-tab port)
                 (anychar-dispatch (+ pos 1) arglist nil))
              ((#\Y)       ; Pretty-print
                 (pretty-print (car arglist) port)
                 ;; IMPLEMENTATION DEPENDENT
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil))
              ((#\F)
                 (require-an-arg arglist)
                 (princ (format-fixed (car arglist) 0 nil) port)
                 (anychar-dispatch (+ pos 1) (cdr arglist) nil))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 ;; gather "~w[,d]F" w and d digits
                 (let loop ((index (+ pos 1))
                            (w-digits (list (char format-strg pos)))
                            (d-digits '())
                            (in-width? T))
                      (if (>= index length-of-format-string)
                          (error
                           (format "FORMAT: improper numeric format directive in ~s" format-strg))
                          (let ( (next-char (char format-strg index)) )
                            (cond
                              ((digit-char-p  next-char)
                               (if in-width?
                                   (loop (+ index 1)
                                         (cons next-char w-digits)
                                         d-digits
                                         in-width?)
                                   (loop (+ index 1)
                                         w-digits
                                         (cons next-char d-digits)
                                         in-width?)))
                              ((char= next-char #\F)
                               (let ((width  (read-from-string
                                              (coerce (reverse w-digits)
                                                      'string)))
                                     (digits (if (zero? (length d-digits))
                                                 nil
                                                 (read-from-string
                                                  (coerce (reverse d-digits)
                                                          'string)))))
                                 (princ (format-fixed (car arglist)
                                                      width
                                                      digits)
                                        port)
                                 (anychar-dispatch (+ index 1)
                                                   (cdr arglist) nil)))
                              ((char= next-char #\,)
                               (if in-width?
                                   (loop (+ index 1)
                                         w-digits
                                         d-digits
                                         nil)
                                   (error
                                    (format "FORMAT: too many commas in directive ~s" format-strg))))
                              (:else (error (format "FORMAT: ~~w.dF directive ill-formed in ~s" format-strg))))))))
              ((#\? #\K)
                 ;; indirection -- take next arg as format string
                 (cond           ;  and following arg as list of format args
                   ((< (length arglist) 2)
                    (error
                     (format "FORMAT: less arguments than specified for ~~?: ~s" arglist)))
                   ((not (string? (car arglist)))
                    (error
                     (format "FORMAT: ~~? requires a string: ~s" (car arglist))))
                   (:else
                    (format-help (car arglist) (cadr arglist) port)
                    (anychar-dispatch (+ pos 1) (cddr arglist) nil))))
              ((#\H)      ; Help
                 (princ (documentation 'format 'function) port)
                 (anychar-dispatch (+ pos 1) arglist T))
              (otherwise
                 (error (format "FORMAT: unknown tilde escape: ~s"
                                (char format-strg pos)))))))))
      (anychar-dispatch 0 arglist nil))))

;; FORMAT
(defun format (&rest args)
  (cond ((null args)
         (error "FORMAT: required format-string argument is missing"))
        ((stringp (car args))
         (apply #'format (cons nil args)))
        ((< (length args) 2)
         (error (format nil
                        "FORMAT: too few arguments ~s"
                        (cons 'format args))) )
        (:else
         (let ((output-port   (car  args))
               (format-string (cadr args))
               (args          (cddr args)) )
           (let* ((port
                   (cond ((and (streamp output-port)
                               (output-stream-p output-port))
                          output-port)
                         ((eq output-port T)
                          *standard-output*)
                         ((eq output-port nil)
                          (make-string-output-stream))
                         (:else
                          (error
                           (format nil
                                   "FORMAT: bad output-port argument: ~s"
                                   output-port))) ) )
                  (return-value
                   (if (eq output-port nil) ;if format into a string
                       (lambda ()
                         (get-output-stream-string port)) ;then return the string
                       (lambda () dont-print)))) ;else do something harmless
             ;; format main
             (let ((unused-args (format-help format-string args port)))
               (if (not (null unused-args))
                   (error
                    (format "FORMAT: unused arguments ~s"
                            unused-args)))
               (funcall return-value)))))))

(test format
  (signals (cl:error)
    (format))
  (is (string= (format "~A" 'foo)
               "FOO"))
  (signals (cl:error)
    (format nil))
  (is (string= (format nil "~A" 'foo)
               "FOO"))
  ;; H
  (is (string= (format "~H")
               (documentation 'format 'function)))
  ;; A
  (is (string= (format "~A" '(1 2 3 4))
               "(1 2 3 4)"))
  ;; S
  (is (string= (format "~S" "FOO")
               "\"FOO\""))
  ;; W
  (is (string= (format "~W" (read-from-string "#0=(1 2 3 . #0#)"))
               "#1=(1 2 3 . #1#)"))
  ;; ~
  (is (string= (format "~~~~~~~~~~~~~~~~")
               "~~~~~~~~"))
  ;; T
  (is (string= (format "~T")
               (string #\Tab)))
  ;; %
  (is (string= (format "~%")
               (string #\Newline)))
  ;; &
  (is (string= (format "A~&")
               (concatenate 'string
                            (string #\A)
                            (string #\Newline))))
  ;; D
  (is (string= (format "~D" 10)
               "10"))
  ;; X
  (is (string= (format "~X" #XFF)
               "FF"))
  ;; O
  (is (string= (format "~O" #O77)
               "77"))
  ;; B
  (is (string= (format "~B" #B1010)
               "1010"))
  ;; F
  (is (string= (format "~F" 1)
               "1"))
  (is (string= (format "~10F" 1)
               "         1"))
  (is (string= (format "~20,10F" 1)
               "        1.0000000000"))
  ;; C
  (is (string= (format "~C" #\A)
               "A"))
  ;; _
  (is (string= (format "~_")
               " "))
  ;; Y
  (is (string= (format "~Y" "Yuppify")
               "\"Yuppify\""))
  ;; ?
  (is (string= (format "~?" "~A~A~A" '(1 2 3))
               "123"))
  ;; K
  (is (string= (format "~K" "~A~A~A" '(1 2 3))
               "123")))
