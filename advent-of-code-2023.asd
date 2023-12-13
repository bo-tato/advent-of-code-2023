;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :depends-on (:serapeum :alexandria :cl-ppcre :str :array-operations :trivia
               :cl-interpol :metabang-bind :fn :series :function-cache)
  :components ((:file "package")
               (:file "advent-of-code-2023")))
