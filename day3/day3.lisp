(in-package :advent-of-code-2023)

(defparameter *grid* (coerce (read-file-lines "input.txt") 'vector))
(defparameter *rows* (length *grid*))
(defparameter *cols* (length (aref *grid* 0)))

(defun adjacent-symbol-p (row col)
  (loop for row across (subseq *grid* (max (1- row) 0) (min (+ row 2) *rows*))
        thereis (loop for char across (subseq row (max (1- col) 0) (min (+ col 2) *cols*))
                      thereis (and (not (digit-char-p char))
                                   (char/= char #\.)))))

(loop for row across *grid*
      for row-num from 0
      sum (loop with after-position = 0 and number
                for col = (position-if #'digit-char-p row :start after-position)
                while col
                do (multiple-value-setq (number after-position)
                     (parse-integer row :start col :junk-allowed t))
                when (loop for col from col below after-position
                           thereis (adjacent-symbol-p row-num col))
                  sum number))
 ; => 535235 (20 bits, #x82AC3)

(defun find-star (row col-start col-end)
  (loop for row from (max (1- row) 0) below (min (+ row 2) *rows*)
        thereis (when-let (col (position-if (eqls #\*) (aref *grid* row)
                                            :start (max (1- col-start) 0)
                                            :end (min (1+ col-end) *cols*)))
                  (list row col))))

(let ((stars (make-array (list *rows* *cols*) :initial-element nil)))
  (loop for row across *grid*
        for row-num from 0
        do (loop with after-position = 0 and number
                 for col = (position-if #'digit-char-p row :start after-position)
                 while col
                 do (multiple-value-setq (number after-position)
                      (parse-integer row :start col :junk-allowed t))
                    (when-let (pos (find-star row-num col after-position))
                      (let ((star (ensure (apply #'aref stars pos) (cons 0 1))))
                        (incf (car star))
                        (multf (cdr star) number)))))
  (loop for (count . sum) across (aops:flatten stars)
        when (and count (= 2 count))
          sum sum))
 ; => 79844424 (27 bits, #x4C25448)
