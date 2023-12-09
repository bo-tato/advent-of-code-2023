(in-package :advent-of-code-2023)

(defun next-value (history-line)
  (loop for nums = (string-to-num-list history-line)
          then (loop for (x y) on nums while y
                     collect (- y x))
        while (notevery #'zerop nums)
        sum (lastcar nums)))

(sum (mapcar #'next-value (read-file-lines "input.txt")))
