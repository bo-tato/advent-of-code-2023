(in-package :advent-of-code-2023)

(defparameter *workflows* (dict))
(setf (@ *workflows* "A") (constantly t))
(setf (@ *workflows* "R") (constantly nil))

(destructuring-bind (workflows parts) (str:split #?"\n\n"
                                                 (str:from-file "input.txt"))
  (dolist (line (lines workflows))
    (let* ((open-brace (position #\{ line))
           (close-brace (position #\} line))
           (workflow (subseq line 0 open-brace))
           (rules (str:split "," line
                             :start (1+ open-brace) :end close-brace)))
      (setf (@ *workflows* workflow)
            (lambda (xmas)
              (dolist (rule (butlast rules)
                            (funcall (@ *workflows* (lastcar rules)) xmas))
                (register-groups-bind (category operator num dest)
                    ("([xmas])([<>])(\\d+):(\\w+)" rule)
                  (when (funcall (match operator
                                   ("<" #'<)
                                   (">" #'>))
                                 (funcall (match category
                                            ("x" #'first)
                                            ("m" #'second)
                                            ("a" #'third)
                                            ("s" #'fourth))
                                          xmas)
                                 (parse-integer num))
                    (return (funcall (@ *workflows* dest) xmas)))))))))
  (~>> (lines parts)
       (mapcar #'string-to-num-list)
       (filter (@ *workflows* "in"))
       flatten
       sum))
