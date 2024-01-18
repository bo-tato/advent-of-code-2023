(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *map* (dict))
(defparameter *part1* 0)

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (setf (@ *map* (cons row col)) char)))

(defun dfs (pos steps visited)
  (if (equal pos '(140 . 139))
      (maxf *part1* steps)
      (loop for neighbor in (case (@ *map* pos)
                              (#\< [(move pos :left)])
                              (#\> [(move pos :right)])
                              (#\^ [(move pos :up)])
                              (#\v [(move pos :down)])
                              (#\. (neighbors pos)))
            when (and (find (@ *map* neighbor) ".<>^v")
                      (not (member neighbor visited :test 'equal)))
              do (dfs neighbor (1+ steps) (cons pos visited)))))

(dfs '(0 . 1) 0 nil)
