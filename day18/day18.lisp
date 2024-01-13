(in-package :advent-of-code-2023)

(defun part1-parse (line)
  (destructuring-bind (direction distance color) (words line)
    (declare (ignore color))
    (list (match direction
            ("U" :up)
            ("D" :down)
            ("R" :right)
            ("L" :left))
          (parse-integer distance))))

(defun part2-parse (line)
  (register-groups-bind (distance direction) ("#(\\p{Hex_Digit}{5})(\\d)" line)
    (list (match direction
            ("0" :right)
            ("1" :down)
            ("2" :left)
            ("3" :up))
          (parse-integer distance :radix 16))))

(loop for position = '(0 . 0) then (move position direction distance)
      for line in (read-file-lines "input.txt")
      for (direction distance) = (part2-parse line)
      collect position into points
      sum distance into perimiter
      finally (return (+ (abs (shoelace points)) (/ perimiter 2) 1)))
