;;;; package.lisp

(defpackage :advent-of-code-2023
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :ppcre
   :register-groups-bind :scan-to-strings :all-matches-as-strings)
  (:import-from :array-operations/utilities
   :multf)
  (:import-from :metabang-bind
   :lambda-bind :bind)
  (:import-from :named-readtables
   :in-readtable)
  (:import-from :series
   :collect-length :until-if :series :collecting-fn)
  (:import-from :trivia
   :if-match :match)
  (:import-from :function-cache
   :clear-cache :defcached))

(named-readtables:defreadtable
    :aoc-sugar
  (:merge
   ;; λ(* 2 _) style lambda shorthand syntax
   :fn.reader
   ;; string interpolation and regex literals
   ;; see: http://edicl.github.io/cl-interpol/
   :interpol-syntax))
