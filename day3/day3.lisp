(in-package :advent-of-code-2023)

(defparameter *grid* (coerce (read-file-lines "input.txt") 'vector))
(defparameter *rows* (length *grid*))
(defparameter *cols* (length (aref *grid* 0)))

(defun find-adjacent (predicate row col-start col-end)
  (loop for row from (max (1- row) 0) below (min (+ row 2) *rows*)
        thereis (when-let (col (position-if predicate (aref *grid* row)
                                            :start (max (1- col-start) 0)
                                            :end (min (1+ col-end) *cols*)))
                  (list row col))))

(defun schematic-symbol-p (char)
  (and (not (digit-char-p char))
       (char/= char #\.)))

(defun solve (part)
  (summing
    (let ((stars (make-array (list *rows* *cols*) :initial-element nil)))
      (loop for row across *grid*
            for row-num from 0
            do (loop with after-position = 0 and number
                     for col = (position-if #'digit-char-p row :start after-position)
                     while col
                     do (multiple-value-setq (number after-position)
                          (parse-integer row :start col :junk-allowed t))
                        (if (eq part :part1)
                            (when (find-adjacent #'schematic-symbol-p row-num col after-position)
                              (sum number))
                            (when-let (pos (find-adjacent (eqls #\*) row-num col after-position))
                              (let ((star (ensure (apply #'aref stars pos) (cons 0 1))))
                                (incf (car star))
                                (multf (cdr star) number))))))
      (when (eq part :part2)
          (loop for (count . gear-ratio) across (aops:flatten stars)
                when (eql 2 count)
                  do (sum gear-ratio))))))

(solve :part1)
(solve :part2)
