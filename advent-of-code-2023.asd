;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :depends-on (:serapeum :fn :alexandria :cl-ppcre :str)
  :components ((:file "package")))
