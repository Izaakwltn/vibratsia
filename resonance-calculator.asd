;;;; resonance-calculator.asd
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(asdf:defsystem #:resonance-calculator
  :version "0.0.2"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A Webapp for exploring resonance on string instruments."
  :depends-on (#:vibratsia #:hunchentoot #:spinneret #:parse-float)
  :serial t
  :components ((:module "resonance-calculator"
                :serial t
                :components ((:file "package")
                             (:file "launch-site")
                             (:file "page")
			     (:file "app")
			     (:file "output")))))
