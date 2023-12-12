(in-package :advent-of-code-2023)

(defun next-value (history-line part)
  (loop for nums = (string-to-num-list history-line)
          then (rest (deltas nums))
        while (notevery #'zerop nums)
        for sign = 1 then (- sign)
        if (eq part :part1)
          sum (lastcar nums)
        else
          sum (* sign (first nums))))

(loop for line in (read-file-lines "input.txt")
      sum (next-value line :part2))
