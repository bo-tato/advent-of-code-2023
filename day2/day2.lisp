(in-package :advent-of-code-2023)

(defun get-color (set color)
  "Return the number of cubes of COLOR in SET."
  (parse-integer (or (scan-to-strings (str:concat "\\d+ " color) set)
                     "0")
                 :junk-allowed t))

(defun set-possible-p (set)
  "Return true if a given SET of cubes is possible given a bag containing 12
red, 13 green, and 14 blue cubes."
  (let ((red (get-color set "red"))
        (blue (get-color set "blue"))
        (green (get-color set "green")))
    (and (<= red 12)
         (<= green 13)
         (<= blue 14))))

(defun game-possible-p (game)
  (every #'set-possible-p (str:split ";" game)))

;; part1
(loop for game in (read-file-lines "input.txt")
      for game-number from 1
      when (game-possible-p game)
        sum game-number)

(defun game-power (game)
  "Return the power of the minimum set of cubes possible for GAME."
  (loop for set in (str:split ";" game)
        maximizing (get-color set "red") into red
        maximizing (get-color set "blue") into blue
        maximizing (get-color set "green") into green
        finally (return (* red blue green))))

;; part2
(loop for game in (read-file-lines "input.txt")
      sum (game-power game))
