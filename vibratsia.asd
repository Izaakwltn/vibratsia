;;;; vibratsia.asd
;;;;
;;;; Copyright (c) 2021-2022 Izaak Walton

(asdf:defsystem #:vibratsia
  :version "0.0.3"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A module for analyzing sympathetic vibrations on stringed instruments."
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "pitch")
	       (:file "resonance")
	       (:file "instruments")
	       (:file "scales")
	       (:file "analysis")))
 
