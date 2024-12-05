(in-package :cl-user)
(ql:quickload "cl-ppcre")
(ql:quickload "fn")
(ql:quickload "metabang-bind")
(use-package :fn)
(use-package :metabang-bind)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun ints-in-str (str)
  (mapcar #'parse-integer
      (cl-ppcre:all-matches-as-strings "\\d+" str)))

(defun build-sorting-fn (order-lines)
  "Given ORDER-LINES, build the ordering function (Integer -> Integer -> Bool).

Assumes that all integers in the domain are in the ordering rules."
  (reduce
      (lambda-bind (fn-exst (a b))
        (lambda (x y)
          (cond
           ((and (= x a) (= y b)) t)
           ((and (= x b) (= y a)) nil)
           (t (funcall fn-exst x y)))))
      (mapcar #'ints-in-str order-lines)
    :initial-value (lambda (a b) (error (format nil "Detected unknown numbers ~a and ~a" a b)))))

(defun main-1 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (order-lines (remove-if-not (fn~ #'find #\|) lines))
         (updates (mapcar #'ints-in-str
                      (remove-if-not (fn~ #'find #\,) lines)))
         (ordering-fn (build-sorting-fn order-lines)))
    (reduce (lambda (sum update)
              (if (equal update (sort (copy-list update) ordering-fn))
                  (+ sum (nth (floor (/ (length update) 2)) update))
                  sum))
        updates
      :initial-value 0)))

(defun main-2 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (order-lines (remove-if-not (fn~ #'find #\|) lines))
         (updates (mapcar #'ints-in-str
                      (remove-if-not (fn~ #'find #\,) lines)))
         (ordering-fn (build-sorting-fn order-lines)))
    (reduce (lambda (sum update)
              (let ((sorted (sort (copy-list update) ordering-fn)))
                (if (equal update sorted)
                    sum
                    (+ sum (nth (floor (/ (length update) 2)) sorted)))))
        updates
      :initial-value 0)))