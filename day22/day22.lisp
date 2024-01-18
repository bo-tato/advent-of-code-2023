(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defun cubes-in-brick (brick)
  (bind ((((x1 y1 z1) (x2 y2 z2)) brick))
    (loop for x from x1 to x2
          append (loop for y from y1 to y2
                       append (loop for z from z1 to z2
                                    collect [x y z])))))

(defun move-down (brick)
  (destructuring-bind ((x1 y1 z1) (x2 y2 z2)) brick
      [[x1 y1 (1- z1)] [x2 y2 (1- z2)]]))

(defun no-conflicts-p (brick bricks)
  (loop with cubes = (cubes-in-brick brick)
        for other in bricks
        never (intersection cubes (cubes-in-brick other) :test 'equal)))

(defun lower-bricks (bricks)
  (loop for brick in bricks
        for other-bricks = (remove brick bricks)
        collect (loop for next = brick then (move-down next)
                      while (and (> (third (first next)) 1)
                                 (> (third (second next)) 1)
                                 (no-conflicts-p (move-down next) other-bricks))
                      finally (return next))))

(defparameter *bricks*
  (repeat-until-stable #'lower-bricks
                       (loop for line in (read-file-lines "input.txt")
                             collect (mapcar #'string-to-num-list (str:split "~" line)))))

(loop for brick in *bricks*
      for other-bricks = (remove brick *bricks*)
      for fall-count = (length (set-difference (repeat-until-stable #'lower-bricks other-bricks)
                                               other-bricks :test 'equal))
      count (zerop fall-count) into part1
      sum fall-count into part2
      finally (format t "part1: ~a, part2: ~a~%" part1 part2))
