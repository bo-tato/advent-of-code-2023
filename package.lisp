;;;; package.lisp

(defpackage :advent-of-code-2023
  (:use :cl)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :serapeum
   :sum
   :~>>)
  (:import-from :alexandria
   :lastcar)
  (:import-from :ppcre
   :all-matches-as-strings)
  (:import-from :fn
                :fn~))
