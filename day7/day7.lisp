(in-package :advent-of-code-2023)

(defconstant +hand-order+ '(:high-card :one-pair :two-pair :three-of-a-kind
                            :full-house :four-of-a-kind :five-of-a-kind))
(defparameter *part* :part2)
(defparameter *card-order* (if (eq *part* :part1)
                               "23456789TJQKA"
                               "J23456789TQKA"))

(defun hand-type (hand)
  (let* ((card-frequencies (hash-table-alist (frequencies hand)))
         (max-frequency (loop for (card . freq) in card-frequencies
                              when (or (eq *part* :part1)
                                       (char/= card #\J))
                                maximize freq))
         (count-jokers (if (eq *part* :part2)
                           (count #\J hand)
                           0)))
    (case (+ max-frequency count-jokers)
      (5 :five-of-a-kind)
      (4 :four-of-a-kind)
      (3 (case count-jokers
           (2 :three-of-a-kind)
           (1 (if (= 2 (count 2 card-frequencies :key #'cdr))
                  :full-house
                  :three-of-a-kind))
           (0 (if (rassoc 2 card-frequencies)
                  :full-house
                  :three-of-a-kind))))
      (2 (if (= 2 (count 2 card-frequencies :key #'cdr))
             :two-pair
             :one-pair))
      (1 :high-card))))

(defun hand< (hand1 hand2)
  (if (eq (hand-type hand1) (hand-type hand2))
      (loop for card1 across hand1
            for card2 across hand2
            when (char/= card1 card2)
              return (< (position card1 *card-order*)
                        (position card2 *card-order*)))
      (< (position (hand-type hand1) +hand-order+)
         (position (hand-type hand2) +hand-order+))))

(loop for (_ bid) in (sort (mapcar #'words (read-file-lines "input.txt"))
                           #'hand< :key #'car)
      for rank from 1
      sum (* rank (parse-integer bid)))
