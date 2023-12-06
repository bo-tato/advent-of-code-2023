(in-package :advent-of-code-2023)

(defun first-last-digit (part str)
  "Returns a two digit number consisting of the first and last digit in STR.
Digit can only be a number if PART is :part1, or can also be spelled out as a word if :part2"
  (let* ((digit (if (eql part :part1)
                   "\\d"
                   "\\d|one|two|three|four|five|six|seven|eight|nine"))
        (digits (all-matches-as-strings digit str)))
    (~>> (str:concat (first digits) (lastcar digits))
         (str:replace-using '("one" "1"
                              "two" "2"
                              "three" "3"
                              "four" "4"
                              "five" "5"
                              "six" "6"
                              "seven" "7"
                              "eight" "8"
                              "nine" "9"))
         parse-integer)))

(loop for line in (read-file-lines "input.txt")
      sum (first-last-digit :part1 line))
