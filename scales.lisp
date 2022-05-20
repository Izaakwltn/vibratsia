;;;; scales.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package #:vibratsia)

(defclass scale ()
  ((root :initarg :root
	 :accessor root)
   (quality :initarg :quality
	    :accessor quality)
   (octnum    :initarg :octnum
	      :accessor octnum)
   (notes     :initarg :notes
	      :accessor notes))
  (:documentation "A scale defined by root, quality, number of octaves, and notes."))

(defmethod print-object ((obj scale) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((root root)
			 (quality quality)
			 (octnum octnum)
                         (notes notes))
            obj
          (format stream "A ~a-~a, ~a octave scale:~a"
		  (first (freq-to-note root))
		  quality
		  octnum
		  (mapcar #'(lambda (note)
			      (list (first (freq-to-note note))
				    note))
			  notes)))))

(defvar mode-list '((major      (0 2 4 5 7 9 11))
		    (ionian     (0 2 4 5 7 9 11))
		    (dorian     (0 2 3 5 7 9 10))
		    (phrygian   (0 1 3 5 7 8 10))
		    (lydian     (0 2 4 6 7 9 11))
		    (mixolydian (0 2 4 5 7 9 10))
		    (aeolian    (0 2 3 5 7 8 10))
		    (nat-min    (0 2 3 5 7 8 10))
		    (mel-min    (0 2 3 5 7 8 9 10 11))
		    (har-min    (0 2 3 5 7 8 11))
		    (locrian    (0 1 3 5 6 8 10))))

(defun scale-build (root quality octnum);maybe make octnum optional, default is 1
  "Builds the notes of a scale for the scale object."
  (cond ((zerop octnum) (list root))
        (t (append (mapcar #'(lambda (interval)
			(freq-adjust root interval))
		    (second (assoc quality mode-list)))
	         (scale-build (* 2 root) quality (- octnum 1))))))

(defun build-scale (root quality octnum) ;maybe make octnum optional, default is 1
  "Takes a root frequency, scale quality, and number of octaves, returns scale object."
  (make-instance 'scale :root root
		        :quality quality
			:octnum octnum
			:notes (scale-build root quality octnum))) 

(defmethod transpose ((scale scale) half-steps)
  "Transposes a given scale by a given interval in half steps."
  (build-scale (freq-adjust (root scale) half-steps)
	       (quality scale)
	       (octnum scale)))

(defmethod relative ((scale scale))
  "Returns the relative major or minor for a scale."
  (build-scale (if (equal (quality scale) 'major)
		   (freq-adjust (root scale) -3)
		   (freq-adjust (root scale) 3))
	       (if (equal (quality scale) 'major)
		   'mel-min
		   'major)
	       (octnum scale)))

(defmethod parallel ((scale scale))
  "Returns the parallel major or minor for a scale."
  (build-scale (root scale)
	       (if (equal (quality scale) 'major)
		   'mel-min
		   'major)
	       (octnum scale)))

;;;Scale Assessment

(defclass scale-assessment ()
  ((scale      :initarg :scale
	       :accessor scale)
   (instr      :initarg :instr
	       :accessor instr)
   (avg-rating :initarg :avg-rating
	       :accessor avg-rating)
   (rank-list  :initarg :rank-list
	       :accessor rank-list))
  (:documentation "An assessment of a scale on a chosen instrument, returning the average resonance ranking and a list of the notes "))

(defmethod print-object ((obj scale-assessment) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((scale scale)
			 (instr instr)
			 (avg-rating avg-rating)
                         (rank-list rank-list))
            obj
          (format stream "~%~%Resonance Assessment for the scale:~%~%~A, as played on the~%~%:~a~%~%
This scale has an average resonance rating of ~a.~%~%Notes Ranked by Resonance:~% ~{~a~%~}"
		  scale
		  instr
		  avg-rating
		  rank-list))))
		  

(defmethod assess-scale ((scale scale) instrument)
  (make-instance 'scale-assessment :scale scale
		                   :instr instrument
				   :avg-rating (avg-resonance (notes scale) instrument)
				   :rank-list (scale-ranking scale instrument)))
				     
(defmethod scale-ranking ((scale scale) instrument)
  "Given a scale and an instrument, returns a list of notes ranked by resonance."
  (mapcar #'(lambda (f)
	      (list (symp-rating f instrument) (make-note f)))
	  (sort (notes scale)
  #'(lambda (freq1 freq2)
    (> (symp-rating freq1 instrument) (symp-rating freq2 instrument))))))
