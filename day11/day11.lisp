(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(let* (rows
       cols
       (galaxies (loop for line in (read-file-lines "input.txt")
                       for row from 0
                       finally (setf rows row)
                       append (loop for char across line
                                    for col from 0
                                    finally (setf cols col)
                                    when (char= char #\#)
                                      collect (cons row col))))
       (empty-rows (set-difference (iota rows) (mapcar #'car galaxies)))
       (empty-cols (set-difference (iota cols) (mapcar #'cdr galaxies))))
  (summing
    (map-combinations (lambda-bind (((row1 . col1) (row2 . col2)))
                        (when (> row1 row2) (rotatef row1 row2))
                        (when (> col1 col2) (rotatef col1 col2))
                        (sum (+ (- row2 row1)
                                (- col2 col1)
                                (count-if λ(< row1 _ row2) empty-rows)
                                (count-if λ(< col1 _ col2) empty-cols))))
                      galaxies
                      :length 2)))
