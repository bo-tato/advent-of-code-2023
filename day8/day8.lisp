(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defun find-path (moves nodes start end-pred)
  (loop for position = start then (if (char= move #\L)
                                      (car (@ nodes position))
                                      (cdr (@ nodes position)))
        for move in (apply #'circular-list (coerce moves 'list))
        for steps from 0
        when (funcall end-pred position)
          return steps))

(bind (((moves node-lines) (str:split #?"\n\n" (str:from-file "input.txt")))
       (nodes (dict)))
  (loop for (node left right) on (ppcre:all-matches-as-strings "\\w+" node-lines) by #'cdddr
        do (setf (@ nodes node) (cons left right)))
  (format t "part1: ~a~%" (find-path moves nodes "AAA" (equals "ZZZ")))
  (~>> (hash-table-keys nodes)
       (filter λ(str:ends-with-p "A" _))
       (mapcar (lambda (position)
                 (find-path moves nodes position λ(str:ends-with-p "Z" _))))
       (apply #'lcm)
       (format t "part2: ~a~%")))
