;pitch.lisp

(in-package #:vibratsia)

;;;;------------------------------------------------------------------------
;;;;Note/Frequency Conversions
;;;;------------------------------------------------------------------------

(defvar note-freq-table '((C . 16.35)
			  (C# . 17.32)
			  ;(Db . 17.32)
			  (D . 18.35)
			  (D# . 19.45)
			  ;(Eb . 19.45)
			  (E . 20.6)
			  (F . 21.83)
			  (F# . 23.12)
			  ;(Gb . 23.12)
			  (G . 24.5)
			  (G# . 25.96)
			  ;(Ab . 25.96)
			  (A . 27.5)
			  (Bb . 29.14)
			  (B . 30.87)))

;;;;Functions to convert note to frequency
(defun octave-shift (freq num-octaves)
  (* freq (expt 2 num-octaves)))

(defun freq-climber (note-freq octaves-up)
  "Adjusts the note-frequency to the proper octave."
  (cond ((zerop octaves-up) note-freq)
	(t (freq-climber (* 2 note-freq) (- octaves-up 1)))))

(defun note-to-freq (note-name octave);;;has to use quoted note-name
  "Takes a note and octave, returns the note's frequency."
  (freq-climber (cdr (assoc note-name note-freq-table)) octave))

;;;;Functions to convert frequency to note

(defun minimize-freq (frequency counter)
  "Minimizes the frequency until it's in the base octave."
  (cond ((< frequency 31) (list frequency counter))
	(t (minimize-freq (/ frequency 2) (+ counter 1)))))

(defun closest-note (freq freq-list)
  "Returns the equal temperament note-name closest to the frequency."
  (loop :with min-note := (car (first freq-list))
	:with min-freq := (cdr (first freq-list))
	:with min-dist := (abs (- freq min-freq))

	:for (note . note-freq) :in (rest freq-list)
	:for dist := (abs (- freq note-freq))
	:when (< dist min-dist)
	  :do (setf min-note note
		    min-freq note-freq
		    min-dist dist)
	:finally (return (values min-note
				 min-dist))))
(defun freq-to-note (freq)
  "Takes a frequency and returns a (note octave) pair."
  (destructuring-bind (canonical-freq octave)
      (minimize-freq freq 0)
  (list (closest-note canonical-freq note-freq-table) octave)))

;;;;------------------------------------------------------------------------
;;;;Note Class
;;;;------------------------------------------------------------------------
(defclass note ()
  ((note-name :initarg :note-name
	      :accessor note-name)
   (octave    :initarg :octave
	      :accessor octave)
   (freq-float :initarg   :freq-float
	       :accessor freq-float))
  (:documentation "A note defined by Note-name, octave number, 
                   and frequency."))

(defmethod print-object ((obj note) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((note-name note-name)
			 (octave octave)
                         (freq-float freq-float))
            obj
          (format stream "~a-~a, Frequency: ~f" note-name octave freq-float))))

;(defgeneric make-a-note (thing))

;(defmethod make-a-note ((thing number))
 ; (make-instance 'note :note-name (first (freq-to-note thing))
;		       :octave (second (freq-to-note thing))
;		       :freq-float thing))

;(defmethod make-a-note ((thing string))
 ; (make-instance 'note :note-name (subseq 0 1 thing)
;		       :octave (subseq 1 2 thing)
;		       :freq-float thing

(defun make-note (frequency)
  "Makes a full note instance from a given frequency."
  (make-instance 'note :note-name (first (freq-to-note frequency))
		       :octave (second (freq-to-note frequency))
		       :freq-float frequency))

;;;;------------------------------------------------------------------------
;;;;Frequency generation
;;;;------------------------------------------------------------------------
;fn = f0 * (a)^n
;f0 = 440
;n = half steps away from fixed note (positive or negative)
;fn = frequency of note n half steps away
;a = 2^1/12

(defun freq-adjust (root interval)
  "Raises or lowers a root frequency by an interval n in half-steps.
    +----------------------------------+
    |                 n            1/12|
    |f(n) = f(0) * (a)  where a = 2    |
    +----------------------------------+"
  (* root (expt (expt 2 (/ 1 12)) interval)))

(defun freq-incr (fixed)
  "Raises a frequency by one half-step, for building chromatic test samples.
   +----------------------------------+
   |                 1           1/12|
   |f(1) = f(0) * (a)  where a = 2    |
   +----------------------------------+"
  (* fixed (expt (expt 2 (/ 1 12)) 1)))

(defun frequency-ladder (min max)
  "Builds a chromatic test-sample within the bounds."
  (cond ((> min max) nil)
	(t (cons min (frequency-ladder (freq-incr min) max)))))
;;;;------------------------------------------------------------------------

