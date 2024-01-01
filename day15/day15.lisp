(in-package :advent-of-code-2023)

(defun hash (str)
  (reduce (lambda (hash char)
            (rem (* 17 (+ hash (char-code char)))
                 256))
          str :initial-value 0))

(~>> (str:from-file "input.txt")
     str:trim
     (str:split ",")
     (mapcar #'hash)
     sum)
