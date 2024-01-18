(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *map* (dict))
(defparameter *max-path* 0)

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (setf (@ *map* (cons row col)) char)))

(defun dfs (pos steps visited)
  (if (equal pos '(140 . 139))
      (maxf *max-path* steps)
      (loop for neighbor in (case (@ *map* pos)
                              (#\< [(move pos :left)])
                              (#\> [(move pos :right)])
                              (#\^ [(move pos :up)])
                              (#\v [(move pos :down)])
                              (#\. (neighbors pos)))
            when (and (find (@ *map* neighbor) ".<>^v")
                      (not (contains? visited neighbor)))
              do (dfs neighbor (1+ steps) (with visited pos)))))

(time (dfs '(0 . 1) 0 (empty-set)))
(format t "part1: ~a~%" *max-path*)

(do-hash-table (pos char *map*)
  (when (find char "<>^v")
    (setf (@ *map* pos) #\.)))

(time (dfs '(0 . 1) 0 (empty-set)))
(format t "part2: ~a~%" *max-path*)
