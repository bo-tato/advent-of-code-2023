(in-package :advent-of-code-2023)

(loop for position = '(0 . 0) then (move position direction distance)
      for line in (read-file-lines "input.txt")
      for (dir-char dist-str) = (words line)
      for direction = (match dir-char
                        ("U" :up)
                        ("D" :down)
                        ("R" :right)
                        ("L" :left))
      for distance = (parse-integer dist-str)
      collect position into points
      sum distance into perimiter
      finally (return (+ (abs (shoelace points)) (/ perimiter 2) 1)))
