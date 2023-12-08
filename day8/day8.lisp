(in-package :advent-of-code-2023)

(loop with (moves node-lines) = (str:split #?"\n\n" (str:from-file "input.txt"))
      with nodes = (dict)
      initially (loop for (node left right) on (ppcre:all-matches-as-strings "\\w+" node-lines) by #'cdddr
                      do (setf (@ nodes node) (cons left right)))
      for position = "AAA" then (if (char= move #\L)
                                    (car (@ nodes position))
                                    (cdr (@ nodes position)))
      for move in (apply #'circular-list (coerce moves 'list))
      for steps from 0
      when (string= position "ZZZ")
        return steps)
