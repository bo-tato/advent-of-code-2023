(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *graph* (make-instance 'graph))

(loop for line in (read-file-lines "input.txt")
      for (node1 . rest) = (words line)
      do (dolist (node2 rest)
           (add-edge *graph* [(intern node1) (intern node2)] 1)))

(format t "part1: ~a~%" (time (graph:min-cut *graph*)))
