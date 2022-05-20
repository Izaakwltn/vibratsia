;;;; resonance-calculator/launch-site.lisp
;;;;
;;;; Copyright (c) 2021-2022 Izaak Walton

(in-package #:resonance-calculator)

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 4242
                 :document-root
                 #p(asdf:system-relative-pathname "resonance-calculator")))

(defun launch ()
  (hunchentoot::start *server*))
