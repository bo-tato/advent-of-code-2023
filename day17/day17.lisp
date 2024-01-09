(in-package :advent-of-code-2023)

(defstruct node row col direction line-length)
(defparameter *graph* (make-instance 'digraph))
(defparameter *start* (make-node :row 0 :col 0 :direction :none :line-length 0))
(defparameter *max-row* 0)
(defparameter *max-col* 0)
(defparameter *node-ids* (make-hash-table :test 'equalp))
(defparameter *id* 0)
(defparameter *heat-loss* (dict))

(defun add-node (node)
  (ensure (@ *node-ids* node) (incf *id*)))

(defun all-nodes ()
  (hash-table-keys *node-ids*))

(defun add-edge (start dest weight)
  (graph:add-edge *graph* (list (add-node start) (add-node dest)) weight))

(defun shortest-path (start dest)
  (nth-value 1
             (graph:shortest-path *graph* (add-node start) (add-node dest))))

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
               do (loop for line-length from 1 upto 3
                        do (loop for direction in '(:up :down :left :right)
                                 do (add-node (make-node :row row :col col
                                                         :direction direction
                                                         :line-length line-length))
                                    (setf (@ *heat-loss* (cons row col))
                                          (digit-value heat-loss))))
                  (maxf *max-col* col))
         (maxf *max-row* row))
(add-node *start*)

(dolist (start (all-nodes))
  (with-slots (row col direction line-length) start
    (loop for (dest edge-direction) in `(((,(1- row) . ,col) :up)
                                         ((,(1+ row) . ,col) :down)
                                         ((,row . ,(1- col)) :left)
                                         ((,row . ,(1+ col)) :right))
          for new-line-length = (if (eq direction edge-direction)
                                    (1+ line-length)
                                    1)
          for weight = (@ *heat-loss* dest)
          do (when (and weight
                        (<= new-line-length 3)
                        (not (opposite-direction-p direction edge-direction)))
               (add-edge start (make-node :row (car dest) :col (cdr dest)
                                          :direction edge-direction
                                          :line-length new-line-length)
                         weight)))))

(loop for direction in '(:right :down)
      minimize (loop for line-length from 1 to 3
                     minimize (shortest-path *start* (make-node
                                                      :row *max-row* :col *max-col*
                                                      :direction direction
                                                      :line-length line-length))))
