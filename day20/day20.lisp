(in-package :advent-of-code-2023)

(defparameter *config* (dict))

(defun update-inputs (module outputs pulse)
  (dolist (output outputs)
    (destructuring-bind (&key type state &allow-other-keys) (@ *config* output)
      (when (eq type :conjuction)
        (setf (@ state module) pulse)))))

(loop for line in (read-file-lines "input.txt")
      for (module outputs) = (str:split " -> " line)
      do (setf (@ *config* (str:trim-left module :char-bag '(#\% #\&)))
               (append (match (str:s-first module)
                         ("%" '(:type :flip-flop
                                :state nil))
                         ("&" `(:type :conjuction
                                :state ,(dict))))
                       `(:outputs ,(str:split ", " outputs)))))

(do-hash-table (module config *config*)
  (update-inputs module (getf config :outputs) :low))

(defun enq-outputs (module outputs pulse queue)
  (dolist (output outputs)
    (enq (cons output pulse) queue))
  (update-inputs module outputs pulse))

(loop with low-pulses = 0
      and high-pulses = 0
      for button-presses from 1
      do (loop with pulses = (queue)
               initially (enq-outputs "broadcaster" (getf (@ *config* "broadcaster") :outputs) :low pulses)
               for (module . pulse) = (deq pulses)
               while module
               do (case pulse
                    (:high (incf high-pulses))
                    (:low (incf low-pulses)))
                  (destructuring-bind (&key type state outputs) (@ *config* module)
                    (when (equal module "ns")
                      (dolist (input '("dc" "rv" "vp" "cq"))
                        (when (eq (@ state input) :high)
                          (format t "found high for: ~a at press ~a~%" input button-presses))))
                    (case type
                      (:flip-flop (when (eq pulse :low)
                                    (enq-outputs module outputs
                                                 (if state :low :high)
                                                 pulses)
                                    (setf (getf (@ *config* module) :state) (not state))))
                      (:conjuction (enq-outputs module outputs
                                                (if (every (eqs :high)
                                                           (hash-table-values state))
                                                    :low
                                                    :high)
                                                pulses)))))
         (when (= button-presses 1000)
           (format t "part1: ~a~%" (* (+ button-presses low-pulses) high-pulses))))

;; part 2
(lcm 3797 3847 3877 4051)
