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

(defun freq-climber (note-freq octaves-up)
  "Adjusts the note-frequency to the proper octave."
  (cond ((zerop octaves-up) note-freq)
	(t (freq-climber (* 2 note-freq) (- octaves-up 1)))))

(defun note-to-freq (note-name octave);;;has to use quoted note-name
  "Takes a note and octave, returns the note's frequency."
  (freq-climber (rest (assoc note-name note-freq-table)) octave))

;;;;Functions to convert frequency to note

(defun minimize-freq (frequency counter)
  "Minimizes the frequency until it's in the base octave."
  (cond ((< frequency 31) (list frequency counter))
	(t (minimize-freq (/ frequency 2) (+ counter 1)))))

(defvar freq-key '())

(defun closest-note (freq freq-list)
  "Returns the closest note to the frequency."
  (cond ((null freq-list) (first freq-key))
	((< (abs (- freq (rest (first freq-list))))
	    (second freq-key))
	 (progn (setq freq-key
		      (list (first (first freq-list))
			    (abs (- freq (rest (first freq-list))))))
		(closest-note freq (rest freq-list))))
	(t (closest-note freq (rest freq-list)))))

(defun freq-to-note (freq)
  "Takes a frequency and returns a (note octave) pair."
  (setq freq-key '(c 20))
  (list (closest-note (first (minimize-freq freq 0)) note-freq-table)
	(second (minimize-freq freq 0))))

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

(defun make-note (frequency)
  "Makes a full note instance from a given frequency."
  (make-instance 'note :note-name (first (freq-to-note frequency))
		       :octave (second (freq-to-note frequency))
		       :freq-float frequency))

;;;;------------------------------------------------------------------------
;;;;Frequency generation
;;;;------------------------------------------------------------------------
(defun freq-incr (base))

(defun frequency-ladder (min max)
  (cond ((> min max) nil)
	(t (cons (freq-incr min) (frequency-ladder (freq-incr min) max)))))

;;;;------------------------------------------------------------------------

