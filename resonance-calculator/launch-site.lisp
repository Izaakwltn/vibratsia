;;;; resonance-calculator/launch-site.lisp
;;;;
;;;; Copyright (c) 2021-2022 Izaak Walton

(in-package #:resonance-calculator)

(defvar *resonance-app*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 4242
		 :document-root
		 (asdf::system-relative-pathname "vibratsia" "resonance-calculator/")))

(defun launch ()
  "Launching the webapp"
  (hunchentoot::start *resonance-app*))
