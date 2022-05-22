;;;; analysis.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package #:vibratsia)

;;; Sympathetic analysis functions

(defun symp-collection (pitch strings)
  "Collects all resonant notes for each string"
  (cond ((null strings) nil)
	(t (append (compare-overtones pitch (first strings))
		   (symp-collection pitch (rest strings))))))

(defmethod freq-rating ((instrument instrument) freq)
  (length (symp-collection freq (mapcar #'freq-float (strings instrument)))))

(defmethod note-rating ((note note) instrument)
  "Takes note and instrument,
  returns the total number of sympathetic vibrations."
  (length (symp-collection (freq-float note) (mapcar #'freq-float (strings instrument)))))

(defun string-symping (pitch strings)
  (cond ((null strings) nil)
	(t (append (list (list (note-name (first strings)) 'string)
			 (mapcar #'make-note (compare-overtones pitch (freq-float (first strings)))))
		   (string-symping pitch (rest strings))))))

(defmethod symp-by-string ((instrument instrument) freq)
  "Compiles a list of sympathetic vibrations organized by string."
  (string-symping freq (strings instrument)))

;;; Resonance Stat Generation

(defclass note-assessment ()
   ((note-obj    :initarg :note-obj
	     :accessor note-obj)
   (instr    :initarg :instr
	     :accessor instr)
   (rating   :initarg :rating
	     :accessor rating)
   (res-list :initarg :res-list
	     :accessor res-list)))

(defmethod assess-note ((instrument instrument) note)
  (make-instance 'note-assessment :note-obj note
		                  :instr instrument
				  :rating (note-rating note instrument)
				  :res-list (symp-by-string
					     instrument (freq-float note))))

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
                          ~%A list of Frequencies by String:~%~%~{~a~%~}"
		  note-obj instr rating
                  (cond ((> rating 20) "highly resonant")
                        ((> rating 10) "moderately resonant")
                        ((<= rating 10) "marginally resonant")) 
		  res-list))))


;;; Instrument Resonance Profile

(defmethod most-resonant ((instrument instrument))
  (loop :with most-res := (first (frequency-ladder (lower-bound instrument)
						   (upper-bound instrument)))
	:for f in (frequency-ladder (lower-bound instrument)
				    (upper-bound instrument))
	:when (> (note-rating (make-note f) instrument)
                 (note-rating (make-note most-res) instrument))
	  :do (setf most-res f)
	:finally (return (make-note most-res))))


(defmethod resonance-ranking ((instrument instrument))
  (mapcar #'(lambda (f)
	      (list (note-rating (make-note f) instrument) (make-note f)))
	  (sort (frequency-ladder (lower-bound instrument)
			          (upper-bound instrument))
  #'(lambda (freq1 freq2)
      (> (note-rating (make-note freq1) instrument) (note-rating (make-note freq2) instrument))))))

(defmethod optimal-keys ((instrument instrument))
  (mapcar #'(lambda (scale)
	      (list (round (avg-resonance instrument (notes scale)))
		    (first (freq-to-note (root scale)))
		    (quality scale)))
	  (sort (mapcar #'(lambda (note) (build-scale note 'major 3))
			(frequency-ladder (lower-bound instrument) (freq-adjust (lower-bound instrument) 11)))
		#'(lambda (scale1 scale2)
		    (> (avg-resonance instrument (notes scale1)) (avg-resonance instrument (notes scale2)))))))

;;; Instrument assessment

(defclass instrument-assessment ()
  ((instrument :initarg :instrument
               :accessor instrument)
   (key-ranks  :initarg :key-ranks
	       :accessor key-ranks)
   (note-ranks  :initarg :note-ranks
	       :accessor note-ranks)))
  
(defmethod print-object ((obj instrument-assessment) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((instrument instrument)
			 (key-ranks key-ranks)
			 (note-ranks note-ranks))
            obj
          (format stream "~%~a~%~%Most Optimal Keys:~{~%~a~}~%~%Note Ranking by Number of Sympathetic Vibrations:~% ~{~a~%~}"
		  instrument key-ranks note-ranks))))

(defmethod assess-instrument ((instrument instrument))
  (make-instance 'instrument-assessment :instrument instrument
		                        :key-ranks (optimal-keys instrument)
		                        :note-ranks (resonance-ranking instrument)))

;;; Excerpt Analysis -in progress (might try to merge with Lilypond parser)

(defmethod avg-resonance ((instrument instrument) freq-list)
  "Takes a set of frequencies, returns avg resonance on a given instrument."
  (float (/ (reduce #'+ (mapcar #'(lambda (f)
			   (note-rating (make-note f)  instrument))
			 freq-list))
	    (length freq-list))))

(defclass excerpt ()
  ((title :initarg :title
	  :initform 'untitled
	  :accessor title)
   (notes :initarg :notes
	  :accessor notes)))

(defmethod print-object ((obj excerpt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (notes notes))
	obj
      (format stream "title: ~a, notes: ~a" title notes))))

;(defclass excerpt-assessment ()
;  ((excerpt-title :initarg :excerpt-title
;		  :accessor excerpt-title)
 ;  (instr         :initarg :instr
;		  :accessor instr)
 ;  (avg-rating    :initarg :avg-rating
;		  :accessor instr)
 ;  (

