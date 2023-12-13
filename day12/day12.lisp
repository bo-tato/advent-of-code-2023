(in-package :advent-of-code-2023)

(defcached arrangements (springs groups &optional (run-length 0))
  (if-match (cons spring springs) springs
    (case spring
      (#\# (if (or (emptyp groups)
                   (>= run-length (first groups)))
               0
               (arrangements springs groups (1+ run-length))))
      (#\. (cond ((zerop run-length)
                  (arrangements springs groups 0))
                 ((eql run-length (first groups))
                  (arrangements springs (rest groups) 0))
                 (t 0)))
      (#\? (+ (arrangements (cons #\# springs) groups run-length)
              (arrangements (cons #\. springs) groups run-length))))
    (if (match groups
          ((list group) (= group run-length))
          (nil (zerop run-length)))
        1
        0)))

;; part1
(loop for (row groups) in (mapcar #'tokens (read-file-lines "input.txt"))
      sum (arrangements (coerce row 'list) (string-to-num-list groups)))

;; part2
(loop for (row groups) in (mapcar #'tokens (read-file-lines "input.txt"))
      do (clear-cache 'arrangements)
      sum (arrangements (coerce (str:join "?" (make-list 5 :initial-element row)) 'list)
                        (repeat-sequence (string-to-num-list groups) 5)))
