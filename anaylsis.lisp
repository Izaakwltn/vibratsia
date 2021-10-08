;;;;analysis.lisp

(in-package #:vibratsia)

;;;;------------------------------------------------------------------------
;;;;Analysis functions
;;;;------------------------------------------------------------------------

;;;generic function for analyze, methodized for note, passage, etc.

;;;;Excerpt Class
(defclass excerpt ()
  ((title :initarg :title
	  :initform untitled
	  :accessor title)
   (notes :initarg :notes
	  :accessor notes)))

(defmethod print-object ((object excerpt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (notes notes))
	obj
      (format stream "title: ~a, notes: ~a" title notes))))




;;;;Resonance Analysis

(defun symp-collection (pitch strings)
  (cond ((null strings) nil)
	(t (append (compare-overtones pitch (first strings))
		   (symp-collection pitch (rest strings))))))

(defun symp-rating (freq instrument)
  (length (symp-collection freq (mapcar #'freq-float (strings instrument)))))

(defun symp-rating-by-note (note octave instrument)
  "Takes a note-name with octave, and the instrument,
  returns the total number of sympathetic vibrations."
  (length (symp-collection  (note-to-freq note octave) (mapcar #'freq-float (strings instrument)))))

(defun string-symping (pitch strings)
  (cond ((null strings) nil)
	(t (append (list (note-name (first strings)) 'string
			 (mapcar #'make-note (compare-overtones pitch (freq-float (first strings)))))
		   (string-symping pitch (rest strings))))))

(defun symp-by-string (pitch instrument)
  "Compiles a list of sympathetic vibrations by string."
  (string-symping pitch (strings instrument)))
						

(defclass note-assessment ()
   ((note-obj    :initarg :note-obj
	     :accessor note-obj)
   (instr    :initarg :instr
	     :accessor instr)
   (rating   :initarg :rating
	     :accessor rating)
   (res-list :initarg :res-list
	     :accessor res-list)))

(defun assess-note (note-name octave instrument)
  (make-instance 'note-assessment :note-obj (make-note (note-to-freq note-name octave))
		                  :instr instrument
				  :rating (symp-rating (note-to-freq note-name octave) instrument)
				  :res-list (symp-by-string
					     (note-to-freq note-name octave) instrument)))

(defmethod print-object ((obj note-assessment) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((note-obj note-obj)
			 (instr instr)
                         (rating rating)
			 (res-list res-list))
            obj
          (format stream "~%The resonant profile of:~%~%~a
                          ~%as played on the: ~%~a~%
                          ~%Sympathetic Vibration Rating: ~a, which is ~a.~%
                          ~%A list of Frequencies by String:~%~%~a"
		  note-obj instr rating
                  (cond ((> rating 20) "highly resonant")
                        ((> rating 10) "moderately resonant")
                        ((<= rating 10) "marginally resonant")) 
		  res-list))))

;;;;------------------------------------------------------------------------
;;;;Excerpt Analysis
;;;;------------------------------------------------------------------------

(defclass excerpt ()
  ((title :initarg :title
	  :initform untitled
	  :accessor title)
   (notes :initarg :notes
	  :accessor notes)))

(defmethod print-object ((object excerpt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (notes notes))
	obj
      (format stream "title: ~a, notes: ~a" title notes))))

(defclass excerpt-assessment ()
  ((excerpt-title :initarg :excerpt-title
		  :accessor excerpt-title)
   (instr         :initarg :instr
		  :accessor instr)
   (avg-rating    :initarg :avg-rating
		  :accessor instr)
   (

