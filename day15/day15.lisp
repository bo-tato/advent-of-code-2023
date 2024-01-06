(in-package :advent-of-code-2023)

(defparameter *steps* (~>> (str:from-file "input.txt")
                           str:trim
                           (str:split ",")))

(defun hash (str)
  (reduce (lambda (hash char)
            (rem (* 17 (+ hash (char-code char)))
                 256))
          str :initial-value 0))

;; part1
(sum (mapcar #'hash *steps*))


(defun score (boxes)
  (loop for box-num from 1
        for box across boxes
        sum (loop for slot-num from 1
                  for (_ . lens) in (reverse box)
                  sum (* box-num slot-num (parse-integer lens)))))

;; part2
(loop with boxes = (make-array 256 :initial-element nil)
      for step in *steps*
      do (register-groups-bind (label operation lens) ("(\\w+)(-|=)(\\d+)?" step)
           (symbol-macrolet ((box (aref boxes (hash label))))
             (match operation
               ("=" (if-let (old-lens (assoc label box :test 'string=))
                      (setf (cdr old-lens) lens)
                      (push (cons label lens) box)))
               ("-" (setf box (remove label box :key #'car :test 'string=))))))
      finally (return (score boxes)))
