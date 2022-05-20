;;;; resonance-calculator/page.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package #:resonance-calculator)

(defmacro with-page ((&key title) &body body)
  `(spinneret::with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:link :rel "stylesheet"
	      :href "https://cdn.simplecss.org/simple.min.css")
       (:title ,title))
      (:body ,@body)
      (:footer "Vibratsia Resonance Calculator - Copyright (c) 2021-2022 "
	       (:a :href "https://www.github.com/Izaakwltn/vibratsia"
		   "Written using Common Lisp")))))
