;;;; instruments.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package #:vibratsia)

;;; The Instrument class

(defclass instrument ()
  ((name :initarg :name
	 :accessor name)
   (strings :initarg :strings
	    :accessor strings)
   (lower-bound :initarg :lower-bound
		:accessor lower-bound)
   (upper-bound :initarg :upper-bound
		:accessor upper-bound))
  (:documentation "A stringed instrument defined by its name, strings, and resonant range."))

(defmethod print-object ((obj instrument) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((name name)
			 (strings strings))
            obj
          (format stream "~a, strings: ~a" name strings))))

                                        ;Also a function is needed for specific lower/upper bounds

(defun luthier (instrument-name string-freqs)
  "Builds an instrument object given the name and the frequencies of the strings."
  (make-instance 'instrument
    :name instrument-name
    :strings (loop :for string :in string-freqs
		   :collect (make-note string))
    :lower-bound (reduce #'min string-freqs)
    :upper-bound (* 3 (reduce #'max string-freqs))))

;;; Predefined Standard Instruments:

(alexandria:define-constant violin-open-strings '(196 293.66 440 659.25) :test 'equal)

(defvar violin (luthier 'violin violin-open-strings))

(alexandria:define-constant viola-open-strings '(130.8 196 293.66 440) :test 'equal)

(defvar viola (luthier 'viola viola-open-strings))

(alexandria:define-constant cello-open-strings '(65.4 98 146.8  220) :test 'equal) 

(defvar cello (luthier 'cello cello-open-strings))

(alexandria:define-constant bass-open-strings '(41.2 55 73.4 98.0) :test 'equal)

(defvar bass (luthier 'bass bass-open-strings))

                                        ; add guitar, ukulele, etc, 5 string violin

;;;Pre-defined unusual instruments:

(defvar hardanger-fiddle-strings (mapcar #'(lambda (note-set)
					     (note-to-freq (first note-set) (second note-set)))
					 '((B 3) (E 4) (B 4) (F# 5)
					   (C# 5) (E 5) (F# 5) (G# 5) (B 5))))

(defvar hardanger-fiddle (luthier 'hardanger-fiddle hardanger-fiddle-strings))				

;;;;Maybe Sitar, arpeggione


