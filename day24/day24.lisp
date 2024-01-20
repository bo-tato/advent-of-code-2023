(in-package :advent-of-code-2023)
(in-readtable :aoc-sugar)

;; from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
(defun line-intersection (line1 line2)
  (bind (((x1 y1 x2 y2) line1)
         ((x3 y3 x4 y4) line2))
    [(/ (- (* (- (* x1 y2) (* y1 x2))
              (- x3 x4))
           (* (- (* x3 y4) (* y3 x4))
              (- x1 x2)))
        (- (* (- x1 x2) (- y3 y4))
           (* (- y1 y2) (- x3 x4))))
    (/ (- (* (- (* x1 y2) (* y1 x2))
             (- y3 y4))
          (* (- (* x3 y4) (* y3 x4))
             (- y1 y2)))
       (- (* (- x1 x2) (- y3 y4))
          (* (- y1 y2) (- x3 x4)))) ]))

(defun correct-direction-p (delta pos1 pos2)
  (if (plusp delta)
      (> pos2 pos1)
      (<= pos2 pos1)))

(defparameter *in* (mapcar #'string-to-num-list (read-file-lines "input.txt")))

;; part1
(summing
  (map-combinations
   (lambda (lines)
     (ignore-errors (bind ((((x1 y1 _ vx1 vy1 _) (x2 y2 _ vx2 vy2 _)) lines)
                           ((intersection-x intersection-y) (line-intersection
                                                             [x1 y1 (+ x1 vx1) (+ y1 vy1)]
                                                             [x2 y2 (+ x2 vx2) (+ y2 vy2)])))
                      (when (and (<= 200000000000000 intersection-x 400000000000000)
                                 (<= 200000000000000 intersection-y 400000000000000)
                                 (every λ(apply #'correct-direction-p _)
                                        [[vx1 x1 intersection-x] [vy1 y1 intersection-y]
                                         [vx2 x2 intersection-x] [vy2 y2 intersection-y]]))
                        (sum 1)))))
   *in*
   :length 2))

;; part2 method from:
;; https://old.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions/keqf8uq
;; https://topaz.github.io/paste/#XQAAAQAUDAAAAAAAAAA0m0pnuFI8c914retSmoIG4DZEdJ50slbD81JvM5mQSTreyJmJdG5ErENvWrbR2IGVD6L23kMHykcRMgYleThe4um56yMUrQ/uHrF3HuwBAoalVRDpkkpZviPXlbzmoSJoN4HPLXXSEz4to1kWUxZqDAP0KgxHB8lNPronrj59GR1o5RqFHlyAaZKCkCt0CT05d5nlzaQEh7vz9YXrWsW2L7GNJy0xlJasMTNGvbHeDPXyJItiHXa3MseDaV2MWdvPtI54e9x9dNc4x0wcP85QR7YrlVSs0zCm5rXLsb1nwhxW3xodqj8NIsH0KEVTE+RsDEuy9p9GD7XMP3k/ijz+cL4XqNUA16WFns+o63OLK8vjoiOK5hNuNhurOMMPFIZW6J4Gcf1a64jhwzu9ISgbCXSSR+Bds+Enp5Nwbt7ZwZ1dQ+Ht3zQ4fZeC6auWvC1ES+fsaFDO3vNSXhoUvOqqnk7jkpTDnwCCI3BDrpwD4ixNe9OOP3MMecfv0uWuZYp7IIgsVCQVXIFVmmhvdhQsZ0FmfBcbvK04YE8RMztc7U2dJ4gWw/yF69/CppBSQCPH4Kn0ZCtn0uYJiJXq9BbA7QiokCY4P+rK9k1S0QqL2nlmI1BqIZkboC3A/kV12oqDIfxCn3sylSN/NDGoXUhFaF+fwn7Q4tfyE9xnARW+3AxntYM6cMwc8ZcyyOlBnrM4iJgPsXteSvwdXl5b8YwEpUc/h+Y5JQp1PnFALM6GLx/q85mWShC+xF6KYfcJ4oWboeIVN9TYKhLU8m+MPFnqitqskmfRvaPb8LfK3OSdRFDZUg5N+wrGfxcdg8EtttL+/94x+9FAVz31BpkHQtwS5aMlUr1TpLphUbzn862x9UwmDlR3vhWBr/OeZ2FlQO3F01yGuH5MRytgVH7GHGDipyh7lXLjHA8L5RjuDDUZa7/gHUYHx0iW3dz7dC2bsSiWBpgAMP5YVdQhJMbVYyhP68Nw1H7hmHqwXB4u8k4QuXHeetPd3Z9lSUG4KSpjwy5ePYMdLaLI2KM1GInxXo3MC3rtKFvEr0NyV7ifJ8YBsmu9h26z8bv8qRN8SFBQ/IPrIcdrQI4AHXDg1hDr2641UlRFVGDTDebMhcKlR2nUEdko6UyNvb6FrmiwQTIGZT7E2Gb+X02v+DoWEaxLcG1Imdt6j/Tl9+PmcCa0P3V7/ucxC9GOAerwTAfcDEAhaUYOco9a0nxdRLDc9qbn8Fc3rvjsvTIoTwgykwqraj6xMWBdsCYs1/+CY+8A
(defun find-velocity (extract-position-f extract-velocity-f)
  (loop with rock-velocities = (iota 2000 :start -1000)
        for (hail-velocity . count) in (hash-table-alist (frequencies *in* :key extract-velocity-f))
        when (> count 1)
          do (filterf rock-velocities (lambda (rock-velocity)
                                        (loop for distance-difference in (~>> (filter (eqls hail-velocity) *in* :key extract-velocity-f)
                                                                              (mapcar extract-position-f)
                                                                              remove-duplicates
                                                                              deltas
                                                                              rest)
                                              always (and (/= rock-velocity hail-velocity)
                                                          (zerop (mod distance-difference (- rock-velocity hail-velocity)))))))
        finally (assert (single rock-velocities))
                (return (first rock-velocities))))

(bind ((vx (find-velocity #'first #'fourth))
       (vy (find-velocity #'second #'fifth))
       (vz (find-velocity #'third #'sixth))
       ((apx apy apz avx avy avz) (first *in*))
       ((bpx bpy bpz bvx bvy bvz) (second *in*))
       (ma (/ (- avy vy) (- avx vx)))
       (mb (/ (- bvy vy) (- bvx vx)))
       (ca (- apy (* ma apx)))
       (cb (- bpy (* mb bpx)))
       (xpos (floor (- cb ca)
                    (- ma mb)))
       (ypos (floor (+ (* ma xpos)
                       ca)))
       (time (floor (- xpos apx)
                    (- avx vx)))
       (zpos (+ apz (* time
                       (- avz vz)))))
  (+ xpos ypos zpos))
