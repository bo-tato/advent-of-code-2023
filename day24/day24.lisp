(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

;; from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
(defun line-intersection (line1 line2)
  (bind (((x1 y1 x2 y2) line1)
         ((x3 y3 x4 y4) line2))
    [(/ (- (* (- (* x1 y2) (* y1 x2))
              (- x3 x4))
           (* (- (* x3 y4) (* y3 x4))
              (- x1 x2)))
        (- (* (- x1 x2) (- y3 y4))
           (* (- y1 y2) (- x3 x4))))
    (/ (- (* (- (* x1 y2) (* y1 x2))
             (- y3 y4))
          (* (- (* x3 y4) (* y3 x4))
             (- y1 y2)))
       (- (* (- x1 x2) (- y3 y4))
          (* (- y1 y2) (- x3 x4)))) ]))

(defun correct-direction-p (delta pos1 pos2)
  (if (plusp delta)
      (> pos2 pos1)
      (<= pos2 pos1)))

(summing
  (map-combinations
   (lambda (lines)
     (ignore-errors (bind ((((x1 y1 _ vx1 vy1 _) (x2 y2 _ vx2 vy2 _)) lines)
                           ((intersection-x intersection-y) (line-intersection
                                                             [x1 y1 (+ x1 vx1) (+ y1 vy1)]
                                                             [x2 y2 (+ x2 vx2) (+ y2 vy2)])))
                      (when (and (<= 200000000000000 intersection-x 400000000000000)
                                 (<= 200000000000000 intersection-y 400000000000000)
                                 (every λ(apply #'correct-direction-p _)
                                        [[vx1 x1 intersection-x] [vy1 y1 intersection-y]
                                         [vx2 x2 intersection-x] [vy2 y2 intersection-y]]))
                        (sum 1)))))
   (mapcar #'string-to-num-list (read-file-lines "input.txt"))
   :length 2))
