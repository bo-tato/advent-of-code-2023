(in-package :advent-of-code-2023)

(defun slide-north (grid)
  (loop for (row next-row) on grid
        while next-row
        do (loop for col below (length row)
                 when (and (char= (aref row col) #\.)
                           (char= (aref next-row col) #\O))
                   do (rotatef (aref row col) (aref next-row col)))))

(defun slide-south (grid)
  (slide-north (reverse grid)))

(defun slide-west (grid)
  (loop for row in grid
        do (loop for col below (1- (length row))
                 when (and (char= (aref row col) #\.)
                           (char= (aref row (1+ col)) #\O))
                   do (rotatef (aref row col) (aref row (1+ col))))))

(defun slide-east (grid)
  (loop for row in grid
        do (loop for col from (1- (length row)) downto 1
                 when (and (char= (aref row col) #\.)
                           (char= (aref row (1- col)) #\O))
                   do (rotatef (aref row col) (aref row (1- col))))))

(defun cycle (grid)
  (let ((rows (length grid))
        (cols (length (first grid))))
    (loop repeat rows do (slide-north grid))
    (loop repeat cols do (slide-west grid))
    (loop repeat rows do (slide-south grid))
    (loop repeat cols do (slide-east grid))))

(defun score (grid)
  (loop for load downfrom (length grid)
        for row in grid
        sum (* load (count #\O row))))

;; part1
(loop with grid = (read-file-lines "input.txt")
      repeat (length grid)
      do (slide-north grid)
      finally (return (score grid)))

;; part2
(loop with grid = (read-file-lines "input.txt")
      with seen-grids = (dict)
      for grid-copy = (loop for row in grid
                            collect (copy-seq row))
      for cycles from 0
      do (if-let (prev-cycle (swaphash grid-copy cycles seen-grids))
           (progn (loop repeat (mod (- 1000000000 cycles)
                                    (- cycles prev-cycle))
                        do (cycle grid))
                  (return (score grid)))
           (cycle grid)))
