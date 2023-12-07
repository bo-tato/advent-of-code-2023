(in-package :advent-of-code-2023)

(defconstant +hand-order+ '(:high-card :one-pair :two-pair :three-of-a-kind
                            :full-house :four-of-a-kind :five-of-a-kind))
(defconstant +card-order+ "23456789TJQKA")

(defun hand-type (hand)
  (let ((card-frequencies (hash-table-alist (frequencies hand))))
    (cond ((rassoc 5 card-frequencies) :five-of-a-kind)
          ((rassoc 4 card-frequencies) :four-of-a-kind)
          ((and (rassoc 3 card-frequencies)
                (rassoc 2 card-frequencies))
           :full-house)
          ((rassoc 3 card-frequencies) :three-of-a-kind)
          ((= 2 (count 2 card-frequencies :key #'cdr)) :two-pair)
          ((rassoc 2 card-frequencies) :one-pair)
          (t :high-card))))

(defun hand< (hand1 hand2)
  (if (eq (hand-type hand1) (hand-type hand2))
      (loop for card1 across hand1
            for card2 across hand2
            when (char/= card1 card2)
              return (< (position card1 +card-order+)
                        (position card2 +card-order+)))
      (< (position (hand-type hand1) +hand-order+)
         (position (hand-type hand2) +hand-order+))))

(loop for (_ bid) in (sort (mapcar #'words (read-file-lines "input.txt"))
                           #'hand< :key #'car)
      for rank from 1
      sum (* rank (parse-integer bid)))
