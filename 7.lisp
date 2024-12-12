(in-package :cl-user)
(ql:quickload "fset")
(ql:quickload "fn")
(ql:quickload "transducers")
(ql:quickload "transducers/fset")
(ql:quickload "cl-ppcre")
(defpackage :aoc-7
  (:use :cl :fn)
  (:local-nicknames
   (#:f #:fset)
   (#:t #:transducers)
   (#:s #:transducers/fset)))
(in-package :aoc-7)

(declaim (optimize (debug 3)))  ;; or C-u C-c C-c to compile with maximal debug settings.

(defun all-capture-groups (regex str)
  "Get a list of all capture groups from REGEX in STR."
  (let ((results nil))
    (cl-ppcre:do-register-groups (capture)
      (regex str)
      (push capture results))
    (nreverse results)))

(defstruct equation
  (result (error "Must supply a result") :type Integer)
  (operands (error "Must supply operands") :type Cons))

(defun parse-equation (line)
  (destructuring-bind (result &rest operands)
      (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" line))
    (make-equation :result result :operands operands)))

(defun valid-equation (equation)
  (labels ((recur (total rem-operands)
             (let ((n (first rem-operands)))
               (if rem-operands
                   (or (recur (+ total n)
                              (rest rem-operands))
                       (recur (* total n)
                              (rest rem-operands))
                       ;; Comment this call out for part 1
                       (recur (+ (* total (expt 10 (1+ (floor (log n 10)))))
                                 n)
                              (rest rem-operands)))
                   (= total
                      (equation-result equation))))))
    (recur 0 (equation-operands equation))))

(defun main-1 (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (t:transduce
     (t:comp (t:map #'parse-equation)
             (t:filter #'valid-equation)
             (t:map #'equation-result))
     #'+
     lines)))
