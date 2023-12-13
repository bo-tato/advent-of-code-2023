(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defun reflectionp (seq1 seq2)
  (if-let (position (mismatch (reverse seq1) seq2))
    (= position
       (min (length seq1) (length seq2)))
    t))

(defun vertical-reflection-p (grid col)
  (loop for row in grid
        always (reflectionp (subseq row 0 col) (subseq row col))))

(defun reflection-position (grid)
  (or (loop for col from 1 below (length (first grid))
         when (vertical-reflection-p grid col)
           return col)
      0))

(loop for grid-string in (str:split #?"\n\n" (str:from-file "input.txt"))
      for grid = (lines grid-string)
      sum (reflection-position grid)
      sum (* 100 (reflection-position (apply #'map 'list #'list grid))))
