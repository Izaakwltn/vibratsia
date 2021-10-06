;;;;instruments.lisp

(in-package #:vibratsia)

;;;;Open Strings

(defconstant violin-open-strings '(196 293.66 440 659.25))

(defconstant viola-open-strings '(130.8 196 293.66 440))

(defconstant cello-open-strings '(65.4 98 146.8  220))

(defconstant bass-open-strings '(41.2 55 73.4 98.0))

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
  (make-instance 'instrument :name instrument-name
		             :strings (loop for string in string-freqs
					    collect (make-note string))))

;VIBRATSIA> (luthier 'violin violin-open-strings)
;#<INSTRUMENT VIOLIN, strings: (#<NOTE G-3, Frequency: 196.0>
 ;                              #<NOTE D-4, Frequency: 293.66>
  ;                             #<NOTE A-4, Frequency: 440.0>
   ;                            #<NOTE E-5, Frequency: 659.25>)> 
				
			      
			      
			  

(:documentation "Instrument specification.")
