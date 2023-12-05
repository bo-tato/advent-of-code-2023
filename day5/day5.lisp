(in-package :advent-of-code-2023)

;; kind of messy write-only code for this day but I'm too lazy to clean it up

(defun transition (val transition)
  (loop for (dest-start source-start range-length) on transition by #'cdddr
        when (< source-start val (+ source-start range-length))
          return (+ dest-start val (- source-start))
        finally (return val)))

(defun range-intersect (x-start x-length y-start y-length)
  "Return two values: a range containing the intersection,
and a list containing ranges of x not covered by y."
  (let ((start (max x-start y-start))
        (end (min (+ x-start x-length) (+ y-start y-length))))
    (if (< start end)
        `((,start ,(- end start))
          ((,x-start ,(- start x-start))
           (,end ,(+ x-start x-length (- end)))))
        `(nil ((,x-start ,x-length))))))

(defun transition-ranges (ranges transition)
  (loop with dest-ranges and source-ranges = ranges
        for (dest-start source-start range-length) on transition by #'cdddr
        do (setf source-ranges
                 (loop for (val-start val-length) in source-ranges
                       for (new-dest new-source) = (range-intersect val-start val-length
                                                                    source-start range-length)
                       when new-dest do (incf (car new-dest) (- dest-start source-start))
                                        (push new-dest dest-ranges)
                       append new-source))
        finally (return (delete-if #'zerop (append dest-ranges source-ranges) :key #'second))))

(destructuring-bind (seeds &rest transitions)
    (~>> (str:from-file "input.txt")
         (str:split #?"\n\n")
         (mapcar #'string-to-num-list))
  (format t "part1: ~a~%"
          (loop for seed in seeds
                minimize (reduce #'transition transitions :initial-value seed)))
  (format t "part2: ~a~%"
          (loop for (location-start) in (reduce #'transition-ranges transitions
                                                :initial-value (batches seeds 2))
                minimize location-start)))
