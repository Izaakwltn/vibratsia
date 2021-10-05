;;;;resound.asd
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(asdf:defsystem #:resound
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "I gotta figure this out"
  :description "A sympathetic vibration calculator for stringed instruments"
  :serial t
  :components ((:module "resound")
	       (:file "package")
	       (:file "instruments")))
 
