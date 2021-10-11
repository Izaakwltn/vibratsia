;;;;instruments.lisp

(in-package #:vibratsia)

;;;;------------------------------------------------------------------------
;;;;Defining the Instrument class
;;;;------------------------------------------------------------------------

(defclass instrument ()
  ((name :initarg :name
	 :accessor name)
   (strings :initarg :strings
	    :accessor strings)))

(defmethod print-object ((obj instrument) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((name name)
			 (strings strings))
            obj
          (format stream "~a, strings: ~a" name strings))))

(defun luthier (instrument-name string-freqs)
  (make-instance 'instrument
    :name instrument-name
    :strings (loop :for string :in string-freqs
		   :collect (make-note string))))
;;;;------------------------------------------------------------------------
;;;;Predefined Standard Instruments:
(alexandria:define-constant violin-open-strings '(196 293.66 440 659.25) :test 'equal)

(defvar violin (luthier 'violin violin-open-strings))

(alexandria:define-constant viola-open-strings '(130.8 196 293.66 440) :test 'equal)

(defvar viola (luthier 'viola viola-open-strings))

(alexandria:define-constant cello-open-strings '(65.4 98 146.8  220) :test 'equal) 

(defvar cello (luthier 'cello cello-open-strings))

(alexandria:define-constant bass-open-strings '(41.2 55 73.4 98.0) :test 'equal)

(defvar bass (luthier 'bass bass-open-strings))


;;;;------------------------------------------------------------------------
;;;;Pre-defined unusual instruments:
;;;;------------------------------------------------------------------------
(defvar hardanger-fiddle-strings (mapcar #'(lambda (note-set)
					     (note-to-freq (first note-set) (second note-set)))
					 '((B 3) (E 4) (B 4) (F# 5)
					   (C# 5) (E 5) (F# 5) (G# 5) (B 5))))

(defvar hardanger-fiddle (luthier 'hardanger-fiddle hardanger-fiddle-strings))				



