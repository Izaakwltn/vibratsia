;;;;instruments.lisp

(in-package #:resound)

;;;;Open Strings

(defconstant violin-open-strings '(196 293.66 440 659.25))
;(mapcar #'note-to-freq '((

(defconstant viola-open-strings '(130.8 196 293.66 440))

(defconstant cello-open-strings '(65.4 98 146.8  220))

(defconstant bass-open-strings '(41.2 55 73.4 98.0))
;(mapcar #'(lambda (note)
;	    (note-to-freq (first note) (second note)))
;	'((E 1) (A 1) (D 2) (G 2))) ;=> (41.2 55 73.4 98.0) 

(defclass open-string () ;;;;maybe change to note and move to pitch.lisp
  ((freq      :initarg freq
	      :accessor freq)
   (note-name :initarg note-name
	      :accessor note-name)
   (octave    :initarg octave
	      :accessor octave))
  
(defgeneric string ()
  (:documentation "Defines a string."))

(defmethod string (pitch);;;
  (make-instance 'open-string :freq pitch
			      :note-name (freq-to-note pitch)))
			     ;:octave ideally should be part of f-t-n

(defmethod string (note octave);;;;i don't think this will work
  (make-instance 'open-string :freq (note-to-freq note octave)
		              :note-name note
			      :octave octave))

(defgeneric fresh-set-of-strings (obj))

(defmethod fresh-set-of-strings (



;;;;Instrument functions
(defclass instrument ()
  ((name    :initarg name
            :accessor name)
   (strings :initarg strings
	    :accessor strings)))

(defgeneric instrument-build ())
				
			      
			      
			   
(defmethod instrument-build (violin)
  (;;;loop through violin-open-strings, make-instance open-string,
                                       ;freq = freq
                                       ;string-name = (freq-to-note freq) 
(defmethod instrument-build-build (viola))

(defmethod instrument (string-set)
  (;;;

(defconstant open-strings '(196 293.66 440 659.25))

(defconstant violin-open-strings '((G-string 196)
		       (D-string 293.66)
		       (A-string 440)
				   (E-string 659.25)))

(:documentation "Instrument callibration.")
