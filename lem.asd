;;;; lem.asd

(asdf:defsystem #:lem
  :description "Describe lem here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "lem")))
