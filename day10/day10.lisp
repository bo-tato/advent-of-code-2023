(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *graph* (dict))
(defparameter *rows* nil)
(defparameter *cols* nil)
(defparameter *start* nil)
(defparameter *loop-graph* (dict))

(defun connected (position)
  (case (@ *graph* position)
    (#\| (list (row- position) (row+ position)))
    (#\- (list (col- position) (col+ position)))
    (#\L (list (row- position) (col+ position)))
    (#\J (list (row- position) (col- position)))
    (#\7 (list (row+ position) (col- position)))
    (#\F (list (row+ position) (col+ position)))))

(defun add-loop (position)
  (flet ((mark (row col) (setf (@ *loop-graph* (cons row col)) t)))
    (let ((row (* 3 (car position)))
          (col (* 3 (cdr position))))
      (case (@ *graph* position)
       (#\S (loop for row from row to (+ 2 row)
                  do (loop for col from col to (+ 2 col)
                           do (mark row col))))
       (#\| (loop for row from row to (+ 2 row)
                  do (mark row (1+ col))))
       (#\- (loop for col from col to (+ 2 col)
                  do (mark (1+ row) col)))
       (#\L (mark row (1+ col)) (mark (1+ row) (+ 2 col)))
       (#\J (mark row (1+ col)) (mark (1+ row) col))
       (#\7 (mark (1+ row) col) (mark (+ 2 row) (1+ col)))
       (#\F (mark (+ 2 row) (1+ col)) (mark (1+ row) (+ 2 col)))))))

(defun loop-bound (x)
  (+ 2 (* 3 x)))

(defun out-of-bounds (position)
  (not (and (<= 0 (car position) (loop-bound *rows*))
            (<= 0 (cdr position) (loop-bound *cols*)))))

(defun flood-loop (position)
  (unless (or (@ *loop-graph* position)
              (out-of-bounds position))
    (setf (@ *loop-graph* position) t)
    (mapc #'flood-loop (neighbors position))))

(defun find-path (start end prev &optional (depth 1))
  (add-loop start)
  (destructuring-bind (&optional neighbor1 neighbor2) (connected start)
    (cond ((equal start end) depth)
          ((equal prev neighbor1) (find-path neighbor2 end start (1+ depth)))
          ((equal prev neighbor2) (find-path neighbor1 end start (1+ depth))))))

(loop for row from 0
      finally (setf *rows* row)
      for line in (read-file-lines "input.txt")
      do (loop for char across line
               for col from 0
               finally (setf *cols* col)
               when (char= char #\S)
                 do (setf *start* (cons row col))
               do (setf (@ *graph* (cons row col)) char)))

(format t "part1: ~a~%"
        (/ (find-path (find-if λ(find *start* (connected _) :test 'equal)
                               (neighbors *start*))
                      *start* *start*)
           2))

(flood-loop (cons (loop-bound *rows*) (loop-bound *cols*)))

(format t "part2: ~a~%"
        (loop for row below *rows*
              sum (loop for col below *cols*
                        count (notany λ(@ *loop-graph* _)
                                      (neighbors (cons (1+ (* 3 row)) (1+ (* 3 col))))))))
