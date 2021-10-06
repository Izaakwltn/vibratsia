;;;;resound.asd
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(asdf:defsystem #:vibratsia
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A module for anaylizing sympathetic vibrations on stringed instruments."
  :serial t
  :components ((:file "package")
	       (:file "pitch")
	       (:file "resonance")
	       (:file "instruments")
	       (:file "analysis")))
 
