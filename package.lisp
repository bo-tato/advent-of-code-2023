;;;; package.lisp

(defpackage :advent-of-code-2023
  (:use :cl)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :serapeum
   :batches
   :ensure
   :eqls
   :summing
   :sum
   :~>>)
  (:import-from :alexandria
   :when-let
   :lastcar)
  (:import-from :ppcre
   :scan-to-strings
   :all-matches-as-strings)
  (:import-from :fn
                :fn~)
  (:import-from :array-operations/utilities
                :multf))
