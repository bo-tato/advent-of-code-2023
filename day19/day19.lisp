(in-package :advent-of-code-2023)

(defparameter *workflows* (dict))
(defparameter *workflow-fns* (dict))
(setf (@ *workflow-fns* "A") (constantly t))
(setf (@ *workflow-fns* "R") (constantly nil))

(defun range-size (min max)
  (if (< min max)
      (1+ (- max min))
      0))

(defun combinations (workflow bounds)
  (if (every #'plusp (mapply #'range-size bounds))
      (match workflow
        ("A" (apply #'* (mapply #'range-size bounds)))
        ("R" 0)
        (otherwise
         (loop with bounds = (copy-tree bounds)
               and rules = (@ *workflows* workflow)
               for (category operator num-str dest) in (butlast rules)
               for num = (parse-integer num-str)
               for bound = (funcall (match category
                                      ("x" #'first)
                                      ("m" #'second)
                                      ("a" #'third)
                                      ("s" #'fourth))
                                    bounds)
               when (string= operator "<")
                 sum (let ((old-max (setf (oldf (cadr bound)) (1- num))))
                       (prog1 (combinations dest bounds)
                         (setf (cadr bound) old-max
                               (car bound) num)))
                   into combinations
               when (string= operator ">")
                 sum (let ((old-min (setf (oldf (car bound)) (1+ num))))
                       (prog1 (combinations dest bounds)
                         (setf (car bound) old-min
                               (cadr bound) num)))
                   into combinations
               finally (return (+ combinations
                                  (combinations (lastcar rules)
                                                bounds))))))
      0))

(destructuring-bind (workflows parts) (str:split #?"\n\n"
                                                 (str:from-file "input.txt"))
  (dolist (line (lines workflows))
    (let* ((open-brace (position #\{ line))
           (close-brace (position #\} line))
           (workflow (subseq line 0 open-brace))
           (rules (str:split "," line
                             :start (1+ open-brace) :end close-brace)))

      (dolist (rule (butlast rules))
        (register-groups-bind (category operator num dest)
            ("([xmas])([<>])(\\d+):(\\w+)" rule)
          (push (list category operator num dest) (@ *workflows* workflow))))
      (push (lastcar rules) (@ *workflows* workflow))
      (nreversef (@ *workflows* workflow))

      (setf (@ *workflow-fns* workflow)
            (lambda (xmas)
              (loop with rules = (@ *workflows* workflow)
                    for (category operator num dest) in (butlast rules)
                    when (funcall (match operator
                                    ("<" #'<)
                                    (">" #'>))
                                  (funcall (match category
                                             ("x" #'first)
                                             ("m" #'second)
                                             ("a" #'third)
                                             ("s" #'fourth))
                                           xmas)
                                  (parse-integer num))
                      return (funcall (@ *workflow-fns* dest) xmas)
                    finally (return (funcall (@ *workflow-fns* (lastcar rules))
                                             xmas)))))))

  (~>> (lines parts)
       (mapcar #'string-to-num-list)
       (filter (@ *workflow-fns* "in"))
       flatten
       sum
       (format t "part1: ~a~%"))

  (format t "part2: ~a~%" (combinations "in" '((1 4000) (1 4000) (1 4000) (1 4000)))))
