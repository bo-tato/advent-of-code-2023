(in-package :advent-of-code-2023)

(defparameter *grid* (dict))

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (setf (@ *grid* (cons row col)) char)))

(defun visit (position direction &optional (visited (dict)) (energized (dict)))
  (unless (swaphash (cons position direction) t visited)
    (when-let (tile (@ *grid* position))
      (setf (@ energized position) t)
      (bind (((row . col) position)
             ((:flet move (direction))
              (visit
               (case direction
                 (:up (cons (1- row) col))
                 (:down (cons (1+ row) col))
                 (:left (cons row (1- col)))
                 (:right (cons row (1+ col))))
               direction
               visited
               energized)))
        (case tile
          (#\. (move direction))
          (#\/ (move (case direction
                       (:right :up)
                       (:up :right)
                       (:down :left)
                       (:left :down))))
          (#\\ (move (case direction
                       (:right :down)
                       (:down :right)
                       (:up :left)
                       (:left :up))))
          (#\| (case direction
                 ((:up :down) (move direction))
                 ((:right :left)
                  (move :up)
                  (move :down))))
          (#\- (case direction
                 ((:right :left) (move direction))
                 ((:up :down)
                  (move :right)
                  (move :left))))))))
  (hash-table-count energized))

;; part 1
(visit '(0 . 0) :right)

;; part 2
(loop with max-row = (apply #'max (mapcar #'car (hash-table-keys *grid*)))
      and max-col = (apply #'max (mapcar #'cdr (hash-table-keys *grid*)))
      for row upto max-row
      for col upto max-col
      maximize (visit (cons row 0) :right)
      maximize (visit (cons row max-col) :left)
      maximize (visit (cons 0 col) :down)
      maximize (visit (cons max-row col) :up))
