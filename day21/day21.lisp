(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *garden* (dict))
(defparameter *start* nil)

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (when (find char "S.")
                    (setf (@ *garden* (cons row col)) t))
                  (when (char= char #\S)
                    (setf *start* (cons row col)))))

(defun wrap (point)
  (destructuring-bind (row . col) point
    (cons (mod row 131) (mod col 131))))

(defun reachable-plots (steps)
  (loop repeat (1+ steps)
        for reached = [*start*] then (remove-duplicates (filter λ(@ *garden* (wrap _))
                                                                (mapcan #'neighbors reached))
                                                        :test 'equal)
        finally (return (length reached))))

;; formula from https://github.com/derailed-dash/Advent-of-Code/blob/master/src/AoC_2023/Dazbo's_Advent_of_Code_2023.ipynb
(defun solve-quadratic (plot-counts steps)
  (let* ((grid-size 131)
         (c (first plot-counts))
         (b (floor (- (* 4 (second plot-counts))
                      (* 3 c)
                      (third plot-counts))
                   2))
         (a (- (second plot-counts)
               c
               b))
         (x (floor (- steps (floor grid-size 2))
                   grid-size)))
    (+ (* a (expt x 2))
       (* b x)
       c)))

;; part1
(reachable-plots 64)

;; part2
(solve-quadratic (mapcar #'reachable-plots '(65 196 327)) 26501365)
