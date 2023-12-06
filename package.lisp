;;;; package.lisp

(defpackage :advent-of-code-2023
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :ppcre
   :scan-to-strings :all-matches-as-strings)
  (:import-from :array-operations/utilities
   :multf))
