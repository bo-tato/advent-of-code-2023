(in-package :advent-of-code-2023)

(defun string-to-num-list (string)
  "Return a list of all numbers in STRING."
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "[-\\d]+" string)))

(defun row+ (point &optional (distance 1))
  "Add DISTANCE to the row of POINT."
  (destructuring-bind (row . col) point
    (cons (+ row distance) col)))

(defun row- (point &optional (distance 1))
  "Subtract DISTANCE from the row of POINT."
  (destructuring-bind (row . col) point
    (cons (- row distance) col)))

(defun col+ (point &optional (distance 1))
  "Add DISTANCE to the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (+ col distance))))

(defun col- (point &optional (distance 1))
  "Subtract DISTANCE from the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (- col distance))))

(defun neighbors (point &key directions)
  "Returns a list of the four points adjacent to POINT."
  (loop for f in '(row+ row- col+ col-)
        for direction in '(:down :up :right :left)
        for neighbor = (funcall f point)
        if directions
          collect (cons neighbor direction)
        else
          collect neighbor))

(defun move (point direction &optional (distance 1))
  "Return the the coordinates DISTANCE away from POINT in DIRECTION."
  (case direction
    (:up (row- point distance))
    (:down (row+ point distance))
    (:left (col- point distance))
    (:right (col+ point distance))))

(defun cross-product (point1 point2)
  (bind (((x1 . y1) point1)
         ((x2 . y2) point2))
    (+ (* x1 y2) (- (* x2 y1)))))

;; https://en.wikipedia.org/wiki/Shoelace_formula
(defun shoelace (points)
  "Compute the area of the polygon defined by POINTS."
  (abs (/ (loop for prev = (lastcar points) then point
                for point in points
                sum (cross-product prev point))
          2)))
