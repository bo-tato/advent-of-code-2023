(in-package :advent-of-code-2023)

(defun string-to-num-list (string)
  (mapcar #'parse-integer (str:words string)))

(defun winning-count (card)
  (~>> (str:split ":" card)
       second
       (str:split "|")
       (mapcar #'string-to-num-list)
       (apply #'intersection)
       length))

;; part1
(loop for card in (read-file-lines "input.txt")
      for winning-count = (winning-count card)
      when (plusp winning-count)
        sum (expt 2 (1- winning-count)))

;; part2
(loop with cards = (read-file-lines "input.txt")
      with num-cards = (length cards)
      with copies-count = (make-array num-cards :initial-element 1)
      for i from 0 below num-cards
      for card in cards
      for score = (winning-count card)
      for copies = (aref copies-count i)
      do (loop for j from (1+ i) below num-cards
               repeat score
               do (incf (aref copies-count j) copies))
      sum copies)
