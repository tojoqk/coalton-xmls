(defsystem #:coalton-xmls
  :description "A Coalton binding for the xmls library"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton #:xmls)
  :serial t
  :pathname "src/"
  :components ((:file "coalton-xmls")))
