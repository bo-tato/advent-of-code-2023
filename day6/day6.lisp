(in-package :advent-of-code-2023)

(defun num-wins (time record)
  (loop for charge from 1 below time
        count (> (* charge (- time charge)) record)))

;; part1
(~>> (read-file-lines "input.txt")
     (mapcar #'string-to-num-list)
     (apply #'mapcar #'num-wins)
     prod)

;; part2
(apply #'num-wins (loop for line in (read-file-lines "input.txt")
                        collect (parse-integer (filter #'digit-char-p line))))
