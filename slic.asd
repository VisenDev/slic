(asdf:defsystem
  "slic"
  :depends-on ("alexandria")
  :serial t
  :components
  ((:file "src/utils")
   (:file "src/lexer")
   (:file "src/parser")))

