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

(defun reflection-position (grid &optional (original-position 0))
  (loop for col from 1 below (length (first grid))
        when (and (/= col original-position)
                  (vertical-reflection-p grid col))
          return col
        finally (return 0)))

(defun smudge (char)
  (case char
    (#\# #\.)
    (#\. #\#)))

(defun smudged-reflection-position (grid original-position)
  (loop for row in grid
        thereis (loop for col below (length row)
                      for char = (aref row col)
                      do (setf (aref row col) (smudge char))
                         (let ((pos (reflection-position grid original-position)))
                           (setf (aref row col) char)
                           (when (plusp pos)
                             (return pos))))
        finally (return 0)))

(loop for grid-string in (str:split #?"\n\n" (str:from-file "input.txt"))
      for grid = (lines grid-string)
      for transposed-grid = (apply #'map 'list #'vector grid)
      for vertical-reflect-pos = (reflection-position grid)
      for horizontal-reflect-pos = (reflection-position transposed-grid)
      sum (smudged-reflection-position grid vertical-reflect-pos) into part2
      sum (* 100
             (smudged-reflection-position transposed-grid horizontal-reflect-pos))
        into part2
      sum vertical-reflect-pos into part1
      sum (* 100 horizontal-reflect-pos) into part1
      finally (format t "part1: ~a~%part2: ~a~%" part1 part2))
