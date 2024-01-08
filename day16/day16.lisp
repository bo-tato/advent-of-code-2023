(in-package :advent-of-code-2023)


(defparameter *grid* (dict))

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (setf (@ *grid* (cons row col)) char)))

(defparameter *visited* (dict))
(defparameter *energized* (dict))

(defun visit (position direction)
  (unless (swaphash (cons position direction) t *visited*)
    (when-let (tile (@ *grid* position))
      (setf (@ *energized* position) t)
      (bind (((row . col) position)
             ((:flet move (direction))
              (visit
               (case direction
                 (:up (cons (1- row) col))
                 (:down (cons (1+ row) col))
                 (:left (cons row (1- col)))
                 (:right (cons row (1+ col))))
               direction)))
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
                  (move :left)))))))))

(visit '(0 . 0) :right)

;; part 1
(hash-table-count *energized*)
