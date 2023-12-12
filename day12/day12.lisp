(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

(defun groups (row)
  (~>> (runs row)
       (filter λ(char= #\# (first-elt _)))
       (mapcar #'length)))

(defun arrangements (row groups)
  (if-let ((unknown (position #\? row)))
    (+ (progn (setf (aref row unknown) #\.)
              (arrangements row groups))
       (prog2 (setf (aref row unknown) #\#)
           (arrangements row groups)
         (setf (aref row unknown) #\?)))
    (if (equal groups (groups row))
        1
        0)))

(loop for (row groups) in (mapcar #'tokens (read-file-lines "input.txt"))
      sum (arrangements row (string-to-num-list groups)))
