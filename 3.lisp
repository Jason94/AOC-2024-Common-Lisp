(in-package :cl-user)
(ql:quickload "cl-ppcre")

(defun all-capture-groups (regex str)
  "Get a list of all capture groups from REGEX in STR."
  (let ((results nil))
    (cl-ppcre:do-register-groups (capture)
      (regex str)
      (push capture results))
    (nreverse results)))

(defun parse-mul (str)
  (mapcar (lambda (mul)
            (mapcar #'parse-integer
                (cl-ppcre:all-matches-as-strings "\\d+" mul)))
      (cl-ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" str)))

(defun main-1 (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (reduce #'+ (mapcar (lambda (ints)
                          (apply #'* ints))
                    (parse-mul (reduce (lambda (a b) (concatenate 'string a b))
                                   lines))))))

(defun main-2 (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (reduce #'+ (mapcar (lambda (ints)
                          (apply #'* ints))
                    (parse-mul (reduce (lambda (a b) (concatenate 'string a b))
                                   lines))))))