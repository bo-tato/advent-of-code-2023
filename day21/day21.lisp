(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defparameter *garden* (dict))
(defparameter *start* nil)

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (when (find char "S.")
                    (setf (@ *garden* (cons row col)) t))
                  (when (char= char #\S)
                    (setf *start* (cons row col)))))

(loop repeat (1+ 64)
      for reached = [*start*] then (remove-duplicates (filter λ(@ *garden* _)
                                                              (mapcan #'neighbors reached))
                                                      :test 'equal)
      finally (return (length reached)))
