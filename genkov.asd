;;;; genkov.asd

(asdf:defsystem #:genkov
  :description "A general-purpose Markov Chain library"
  :author "Riccardo Cariboni <info@riccardocariboni.it>"
  :license  "@2024 Riccardo Cariboni"
  :version "0.0.1"
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "genkov-utils")
               (:file "genkov")))
