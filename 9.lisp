(in-package :cl-user)
(ql:quickload "fset")
(ql:quickload "fn")
(ql:quickload "transducers")
(ql:quickload "transducers/fset")
(defpackage :aoc-9
  (:use :cl :fn)
  (:local-nicknames
   (#:f #:fset)
   (#:t #:transducers)
   (#:s #:transducers/fset)))
(in-package :aoc-9)

(defun map-index (f)
  "Transducer that provides the element and its index to F. Calls: (f <element> <index>)"
  (let ((index 0))
    (lambda (reducer)
      (lambda (result &optional (input nil i-p))
        (if i-p
            (progn
              (incf index)
              (funcall reducer result (funcall f input (1- index))))
            (funcall reducer result))))))

(defun make-layout (disk-map)
  (t:transduce
   (t:comp (t:filter-map #'digit-char-p)
           (map-index
            (lambda (n i)
              (make-array n
                           :initial-element (if (oddp i)
                                                "."
                                                (write-to-string (/ i 2))))))
           #'t:concatenate)
   #'t:vector
   disk-map))
;; (sort-layout (make-layout "12345"))
(defun sort-layout (layout)
  (labels ((recur (source-i target-i)
             (if (> source-i target-i)
                 nil
                 (cond
                   ((string/= "." (elt layout source-i))
                    (recur (1+ source-i) target-i))
                   ((string/= "." (elt layout target-i))
                    (recur source-i (1- target-i)))
                   (t
                    (setf (elt layout source-i)
                          (elt layout target-i))
                    (setf (elt layout target-i)
                          ".")
                    (recur (1+ source-i) (1- target-i)))))))
    (recur 0 (1- (length layout)))
    layout))

(defun calc-checksum (sorted-layout)
  (t:transduce
   (t:comp (t:filter-map #'parse-integer)
           (map-index #'*))
   #'+
   sorted-layout))

(defun main-1 (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (calc-checksum
     (sort-layout (make-layout (first lines))))))

;; (main-1 "9_data.txt")