(in-package :cl-user)
(ql:quickload "cl-ppcre")
(ql:quickload "serapeum")

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun valid-report-p (report)
  "Solve REPORT that is a list of integers."
  (labels ((rec (remaining a b increasing)
                (cond
                 ((> (abs (- a b)) 3)
                   (return-from valid-report-p nil))
                 ((and increasing (>= a b))
                   (return-from valid-report-p nil))
                 ((and (not increasing) (<= a b))
                   (return-from valid-report-p nil))
                 ((not remaining) t)
                 (t (rec (rest remaining)
                         b
                         (first remaining)
                         increasing)))))
    (rec (rest (rest report))
         (first report)
         (second report)
         (< (first report)
            (second report)))))

(defun main-1 (filename)
  (let ((reports
         (mapcar (lambda (line)
                   (mapcar #'parse-integer
                       (ppcre:split "\\s+" line)))
             (uiop:read-file-lines filename))))
    (count t (mapcar #'valid-report-p reports))))

(defun valid-report-p2 (report)
  "Solve REPORT that is a list of integers."
  (labels ((rec (remaining a b increasing second-chance-left last-item)
                (cond
                 ((or
                   (> (abs (- a b)) 3)
                   (and increasing (>= a b))
                   (and (not increasing) (<= a b)))
                   (if second-chance-left
                       (if last-item
                           (or
                            (rec remaining
                                 last-item
                                 a
                                 increasing
                                 nil
                                 nil)
                            (rec remaining
                                 last-item
                                 b
                                 increasing
                                 nil
                                 nil))
                           (or
                            (rec
                             (rest remaining)
                             b
                             (first remaining)
                             (< b (first remaining))
                             nil
                             nil)
                            (rec
                             (rest remaining)
                             a
                             (first remaining)
                             (< a (first remaining))
                             nil
                             nil)))
                       nil))
                 ((not remaining) t)
                 (t (rec (rest remaining)
                         b
                         (first remaining)
                         increasing
                         second-chance-left
                         a)))))
    (rec (rest (rest report))
         (first report)
         (second report)
         (< (first report)
            (second report))
         t
         nil)))

(defun remove-n (n list)
  (append (serapeum:firstn n list)
    (serapeum:nthrest (1+ n) list)))

(defun valid-report-p22 (report)
  (if (valid-report-p report)
      t
      (reduce (lambda (a b) (or a b))
          (loop for i from 0 to (1- (length report))
                  collecting (valid-report-p (remove-n i report))))))

(defun main-2 (filename)
  (let ((reports
         (mapcar (lambda (line)
                   (mapcar #'parse-integer
                       (ppcre:split "\\s+" line)))
             (uiop:read-file-lines filename))))
    (dolist (report reports)
      (when (not (equal (valid-report-p2 report)
                        (valid-report-p22 report)))
            (format t "~a: ~a, ~a~%" report (valid-report-p2 report) (valid-report-p22 report))))
    (count t (mapcar #'valid-report-p22 reports))))
