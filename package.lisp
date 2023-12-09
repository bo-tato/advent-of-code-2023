;;;; package.lisp

(defpackage :advent-of-code-2023
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :ppcre
   :scan-to-strings :all-matches-as-strings)
  (:import-from :array-operations/utilities
   :multf)
  (:import-from :metabang-bind
   :bind)
  (:import-from :named-readtables
                :in-readtable))

(named-readtables:defreadtable
    :aoc-sugar
  (:merge
   ;; some convenient reader macros for currying and composition
   ;; see: https://eschulte.github.io/curry-compose-reader-macros
   :fn.reader
   ;; string interpolation and regex literals
   ;; see: http://edicl.github.io/cl-interpol/
   :interpol-syntax))
