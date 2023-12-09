(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

;; alternative solution with the old-fashioned series library that allows
;; operating on infinite sequences
;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node347.html
(defun find-path (moves nodes start end-pred)
  (1+ (collect-length
       (until-if end-pred
                 (collecting-fn 'string λstart
                                (lambda (position move)
                                  (if (char= move #\L)
                                      (car (@ nodes position))
                                      (cdr (@ nodes position))))
                                (apply #'series (coerce moves 'list)))))))

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
