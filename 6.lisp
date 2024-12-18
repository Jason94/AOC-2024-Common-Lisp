(in-package :cl-user)
(ql:quickload "alexandria")
(ql:quickload "array-operations")
(ql:quickload "fset")
(use-package :array-operations/transforming)
(use-package :array-operations/utilities)

(defun str-list-to-2d-array (list-of-strs)
  (let* ((height (length list-of-strs))
         (width (length (first list-of-strs)))
         (grid (make-array (list height width) :element-type 'character)))
    (loop for str in list-of-strs
          for y from 0 to (1- height)
          do (loop for c across str
                   for x from 0 to (1- width)
                   do (setf (aref grid y x) c)))
    grid))

(defun inside-grid-p (grid vec)
  "Check if VEC is inside GRID."
  (let ((x (elt vec 0))
        (y (elt vec 1))
        (width (array-dimension grid 1))
        (height (array-dimension grid 0)))
    (and (>= x 0) (>= y 0)
         (< x width) (< y height))))

(defun extract-starting-position! (grid)
  "Given GRID, remove the starting position (^) and return the starting position."
  (nested-loop (y x) (array-dimensions grid)
    (when (char= (aref grid y x) #\^)
      (setf (aref grid y x) #\.)
      (return-from extract-starting-position! (vector x y))))
  (error "Could not find starting position in grid"))

(defun rotate-vector (vec deg)
  "Rotate VEC by DEG."
  (let ((rad (/ (* deg pi) 180))
        (x (elt vec 0))
        (y (elt vec 1)))
    (each #'round (vector (- (* x (cos rad)) (* y (sin rad)))
                          (+ (* x (sin rad)) (* y (cos rad)))))))

(defun main-1 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (grid (str-list-to-2d-array lines))
         (start (extract-starting-position! grid)))
    (labels ((recur (position facing all-positions)
               (let ((new-pos (each #'+ position facing)))
                 (if (inside-grid-p grid new-pos)
                     (let ((new-char (aref grid (elt new-pos 1) (elt new-pos 0))))
                       (if (char= new-char #\#)
                           (recur position (rotate-vector facing 90) all-positions)
                           (recur new-pos facing (fset:with all-positions position))))
                       (fset:size (fset:with all-positions position))))))
      (recur start (vector 0 -1) (fset:empty-set)))))

(defun causes-cycle-p (grid start pos)
  "Return T if putting a wall at position POS (vector x y) would create a cycle.
Also works if already has a wall. Make sure to pass in a copy of GRID!"
  (setf (aref grid (second pos) (first pos)) #\#)


(defun main-2 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (grid (str-list-to-2d-array lines))
         (start (extract-starting-position! grid)))
    (causes-cycle-p (alexandria:copy-array grid) start (vector 0 0))))
