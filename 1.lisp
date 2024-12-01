(in-package :cl-user)
(ql:quickload "cl-ppcre")

;;
;; Step 1
;;

(defun parse-file-lines (lines)
  "Parse LINES from a file into the two lists of numbers. Returns as a list of two lists."
  (apply #'mapcar
      (cons #'list
            (mapcar (lambda (sublist)
                      (mapcar #'parse-integer sublist))
                (mapcar (lambda (line)
                          (ppcre:split "\\s+" line))
                    lines)))))

(defun main-1 (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (reduce #'+
        (mapcar #'abs
            (apply #'mapcar
                (cons #'- (mapcar (lambda (list) (sort list #'<))
                              (parse-file-lines lines))))))))

;;
;; Step 2
;;

(defun main-2 (filename)
  (let* ((lists (parse-file-lines (uiop:read-file-lines filename)))
         (source (first lists))
         (check (second lists)))
    (reduce #'+
        (mapcar (lambda (x)
                  (* x (count x check)))
            source))))

