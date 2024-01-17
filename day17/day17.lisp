(in-package :advent-of-code-2023)

(defparameter *part* :part1)
(defparameter *min-length* (if (eq *part* :part1)
                               1
                               4))
(defparameter *max-length* (if (eq *part* :part1)
                               3
                               10))

(defstruct node position direction line-length)
(defparameter *graph* (make-instance 'digraph))
(defparameter *start* (make-node :position (cons 0 0) :direction :none :line-length 0))
(defparameter *max-row* 0)
(defparameter *max-col* 0)
(defparameter *node-ids* (make-hash-table :test 'equalp))
(defparameter *id* 0)
(defparameter *heat-loss* (dict))

(defun add-node (node)
  (let ((id (ensure (@ *node-ids* node) (incf *id*))))
    (graph:add-node *graph* id)
    id))

(defun all-nodes ()
  (hash-table-keys *node-ids*))

(defun add-edge (start dest weight)
  (graph:add-edge *graph* [(add-node start) (add-node dest)] weight))

(defun shortest-path (start dest)
  (or (nth-value 1
              (graph:shortest-path *graph* (add-node start) (add-node dest)))
      most-positive-fixnum))

(defun opposite-direction-p (direction1 direction2)
  (eq direction1
      (case direction2
        (:up :down)
        (:down :up)
        (:left :right)
        (:right :left))))

(loop for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for heat-loss across line
               for col from 0
               for position = (cons row col)
               do (loop for line-length from 1 upto *max-length*
                        do (loop for direction in '(:up :down :left :right)
                                 do (add-node (make-node :position position
                                                         :direction direction
                                                         :line-length line-length))
                                    (setf (@ *heat-loss* position)
                                          (digit-value heat-loss))))
                  (maxf *max-col* col))
         (maxf *max-row* row))
(add-node *start*)

(dolist (start (all-nodes))
  (with-slots (position direction line-length) start
    (loop for (dest . edge-direction) in (neighbors position :directions t)
          for new-line-length = (if (eq direction edge-direction)
                                    (1+ line-length)
                                    1)
          for weight = (@ *heat-loss* dest)
          do (when (and weight
                        (<= new-line-length *max-length*)
                        (not (opposite-direction-p direction edge-direction))
                        (or (eq *part* :part1)
                            (eq direction edge-direction)
                            (eq direction :none)
                            (>= line-length *min-length*)))
               (add-edge start (make-node :position dest
                                          :direction edge-direction
                                          :line-length new-line-length)
                         weight)))))

(format t "~a, path: ~a~%" *part*
        (loop for direction in '(:right :down)
              minimize (loop for line-length from *min-length* to *max-length*
                             minimize (shortest-path *start* (make-node
                                                              :position (cons *max-row* *max-col*)
                                                              :direction direction
                                                              :line-length line-length)))))
