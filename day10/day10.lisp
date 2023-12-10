(in-package :advent-of-code-2023)

(defparameter *graph* (dict))
(defparameter *start* '(2 . 0))

(defun neighbors (position)
  (destructuring-bind (row . col) position
    (case (@ *graph* position)
      (#\| (list (cons (1- row) col) (cons (1+ row) col)))
      (#\- (list (cons row (1- col)) (cons row (1+ col))))
      (#\L (list (cons (1- row) col) (cons row (1+ col))))
      (#\J (list (cons (1- row) col) (cons row (1- col))))
      (#\7 (list (cons (1+ row) col) (cons row (1- col))))
      (#\F (list (cons (1+ row) col) (cons row (1+ col)))))))

(defun find-path (start end prev &optional (depth 1))
  (destructuring-bind (&optional neighbor1 neighbor2) (neighbors start)
    (cond ((equal start end) depth)
          ((equal prev neighbor1) (find-path neighbor2 end start (1+ depth)))
          ((equal prev neighbor2) (find-path neighbor1 end start (1+ depth))))))

(loop for row from 0
      for line in (read-file-lines "input.txt")
      do (loop for char across line
               for col from 0
               when (char= char #\S)
                 do (setf *start* (cons row col))
               do (setf (@ *graph* (cons row col)) char)))

(/ (find-path (loop with (start-row . start-col) = *start*
                    for (row-delta col-delta) in '((-1 0) (1 0) (0 -1) (0 1))
                    for neighbor = (cons (+ row-delta start-row) (+ col-delta start-col))
                    when (find *start* (neighbors neighbor) :test 'equal)
                      return neighbor)
              *start*
              *start*)
   2)
