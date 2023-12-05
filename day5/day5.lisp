(in-package :advent-of-code-2023)

(defun transition (val transition)
  (loop for (dest-start source-start range-length) on transition by #'cdddr
        when (< source-start val (+ source-start range-length))
          return (+ dest-start val (- source-start))
        finally (return val)))

(destructuring-bind (seeds &rest transitions)
    (~>> (str:from-file "input.txt")
         (str:split #?"\n\n")
         (mapcar #'string-to-num-list))
  (loop for seed in seeds
        minimize (reduce #'transition transitions :initial-value seed)))
