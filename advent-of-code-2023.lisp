(in-package :advent-of-code-2023)

(defun string-to-num-list (string)
  "Return a list of all numbers in STRING."
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "[-\\d]+" string)))

(defun row+ (point)
  "Add 1 to the row of POINT."
  (destructuring-bind (row . col) point
    (cons (1+ row) col)))

(defun row- (point)
  "Subtract 1 from the row of POINT."
  (destructuring-bind (row . col) point
    (cons (1- row) col)))

(defun col+ (point)
  "Add 1 to the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (1+ col))))

(defun col- (point)
  "Subtract 1 from the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (1- col))))

(defun neighbors (point &key directions)
  "Returns a list of the four points adjacent to POINT."
  (loop for f in '(row+ row- col+ col-)
        for direction in '(:down :up :right :left)
        for neighbor = (funcall f point)
        if directions
          collect (cons neighbor direction)
        else
          collect neighbor))

(defun neighbor (point direction)
  "Return the neighbor of POINT in the given DIRECTION."
  (case direction
    (:up (row- point))
    (:down (row+ point))
    (:left (col- point))
    (:right (col+ point))))
