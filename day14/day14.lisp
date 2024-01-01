(in-package :advent-of-code-2023)

(defun slide-north (grid)
  (loop for (row next-row) on grid
        while next-row
        do (loop for col below (length row)
                 when (and (char= (aref row col) #\.)
                           (char= (aref next-row col) #\O))
                   do (rotatef (aref row col) (aref next-row col)))))

(defun score (grid)
  (loop for load downfrom (length grid)
        for row in grid
        sum (* load (count #\O row))))

(loop with grid = (read-file-lines "input.txt")
      repeat (length grid)
      do (slide-north grid)
      finally (return (score grid)))
