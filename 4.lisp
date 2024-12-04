(in-package :cl-user)
(ql:quickload "cl-ppcre")

(declaim (optimize (speed 0) (space 0) (debug 3)))

; (defun count-xmas-front-to-back (str-list)
;   (loop for string in str-list
;           summing (length (cl-ppcre:all-matches "(XMAS|SAMX)" string))))

; (defun collect-vertical-slices (line-grid)
;   (let ((width (length (elt line-grid 0)))
;         (height (length line-grid)))
;     (loop for x from 0 to (1- width)
;             collecting (coerce
;                          (loop for y from 0 to (1- height)
;                                  collecting (elt (elt line-grid y) x))
;                          'string))))

; (defun collect-angled-slices (line-grid)
;   (let ((height (length line-grid)))
;     (loop for y from 0 to (1- height)
;             appending (list
;                       ;  (coerce
;                       ;    (loop for x from 0 to y
;                       ;            collecting (elt (elt line-grid (- y x)) x))
;                       ;    'string)
;                        (coerce
;                          (loop for x from 1 to y
;                                  collecting (elt (elt line-grid (- height x)) x))
;                          'string)))))

; (defun main-1 (filename)
;   (let* ((lines (uiop:read-file-lines filename))
;          (line-grid (coerce lines '(vector string))))
;     (+
;      (count-xmas-front-to-back lines)
;      (count-xmas-front-to-back
;       (collect-vertical-slices line-grid)))
;     (collect-angled-slices line-grid)))

(defun scan-from-pos (line-grid x y)
  "Scan LINE-GRID from X, Y and return the strings found."
  (let* ((height (length line-grid))
         (width (length (elt line-grid 0)))
         (scan-left (>= x 3))
         (scan-right (>= (- width x 1) 3))
         (scan-up (>= y 3))
         (scan-down (>= (- height y 1) 3)))
    (remove-if-not (lambda (s) (string= s "XMAS"))
        (mapcar (lambda (lst) (coerce lst 'string))
            (list
             (when scan-left
                   (loop for i from 0 to 3 collect (elt (elt line-grid y) (- x i))))
             (when scan-right
                   (loop for i from 0 to 3 collect (elt (elt line-grid y) (+ x i))))
             (when scan-up
                   (loop for i from 0 to 3 collect (elt (elt line-grid (- y i)) x)))
             (when scan-down
                   (loop for i from 0 to 3 collect (elt (elt line-grid (+ y i)) x)))
             (when (and scan-left scan-up)
                   (loop for i from 0 to 3 collect (elt (elt line-grid (- y i)) (- x i))))
             (when (and scan-left scan-down)
                   (loop for i from 0 to 3 collect (elt (elt line-grid (+ y i)) (- x i))))
             (when (and scan-right scan-up)
                   (loop for i from 0 to 3 collect (elt (elt line-grid (- y i)) (+ x i))))
             (when (and scan-right scan-down)
                   (loop for i from 0 to 3 collect (elt (elt line-grid (+ y i)) (+ x i)))))))))

(defun main-1 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (line-grid (coerce lines '(vector string)))
         (height (length line-grid))
         (width (length (elt line-grid 0))))
    (length
      (loop for y from 0 to (1- height)
              appending (loop for x from 0 to (1- width)
                                appending (scan-from-pos line-grid x y))))))

(defun scan-xmas-from-pos (line-grid x y)
  "Scan LINE-GRID from X, Y and return 1 if found and 0 otherwise. Assumes that X and Y are not on the
edge of LINE-GRID."
  (let ((str1 (coerce (list
                       (elt (elt line-grid (1- x)) (1- y))
                       (elt (elt line-grid x) y)
                       (elt (elt line-grid (1+ x)) (1+ y)))
                      'string))
        (str2 (coerce (list
                       (elt (elt line-grid (1+ x)) (1- y))
                       (elt (elt line-grid x) y)
                       (elt (elt line-grid (1- x)) (1+ y)))
                      'string)))
    (if (and (or (string= "MAS" str1)
                 (string= "SAM" str1))
             (or (string= "MAS" str2)
                 (string= "SAM" str2)))
        1
        0)))

(defun main-2 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (line-grid (coerce lines '(vector string)))
         (height (length line-grid))
         (width (length (elt line-grid 0))))
    (loop for y from 1 to (- height 2)
            sum (loop for x from 1 to (- width 2)
                        sum (scan-xmas-from-pos line-grid x y)))))
