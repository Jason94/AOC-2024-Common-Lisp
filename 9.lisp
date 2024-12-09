
(in-package :cl-user)
(ql:quickload "cl-ppcre")

(defun generate-array (input)
  (make-array (reduce #'+ (map 'vector #'digit-char-p input))
              :initial-element #\.)
  (loop with arr = (make-array (reduce #'+ (map 'vector #'digit-char-p input)))
        repeat (length arr)
        for :w

(defun main-1 (filename)
  (let ((input (first (uiop:read-file-lines filename))))
    (generate-array input)))

(length (main-1 "9_test.txt"))
