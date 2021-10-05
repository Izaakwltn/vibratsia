;;;;------------------------------------------------------------------------
;;;;Resonance Calculator in Lisp
;;;;------------------------------------------------------------------------
;;;A set of functions for calculating resonance in violin intonation

;;;tl;dr: compares overtones of a given note with overtones on open strings
;;;to show common resonant frequencies that result in sympathetic vibration

;;;Goals: scheme-lilypond extension (color-coded resonant notes)
	 ;incorporate into realtime tuner (if possible)
	 ;indicate whether the note should be played high or low
                  ;to accomodate optimal resonance. (5 cents sharp etc.)
 

;;;------------------------------------------------------------------------
;;;Current highest-level functionality:

      ;(violin 'a 4)
;note and octave => sympathetic vibration list
      ;(symp-rating 'a 4)
;note and octave => number of sympathetic nodes


      ;(open-sympathizer 220 violin-open-strings)
;;;;output: the sympathetic vibrating notes on each string in relation
;;;to the given frequency.

      ;(how-sympathetic 220)
;for the number of shared harmonics with open strings

;;;;------------------------------------------------------------------------
;;;;package.lisp
;(defpackage #:resound
 ; (:use #:cl)
;
 ; ;;;;resonance.lisp
  ;(:export) ;;;overtone functions, resonance check functions, but more basic
;
  ;;;;instruments.lisp
 ;  (:export)

;etc
;(:;documentation "Resonance Calculation Package."))
;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;pitch.lisp

;(defclass note ()
 ; ((note-name :initarg note-name
;	      :accessor note-name)
 ;  (octave :initarg octave
;	   :accessor octave)
 ;  (frequency :initarg freq
;	      :accessor freq))
 ; (:documentation "Defines a Note."))


;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;resonance.lisp


;(defgeneric fundamental ()) ;either a note or a

;(defmethod fundamental (pitch)
 ; ((


;;;------------------------------------------------------------------------
;;;Open string frequencies- using standard tuning but can be altered

;;;;------------------------------------------------------------------------
;instruments.lisp

;(in-package #:resound)

;(defgeneric open-string ())

;(defgeneric open-strings ())
    
;(defconstant open-strings '(196 293.66 440 659.25))

;(defconstant viola-open-strings '())

;(defconstant cello-open-strings '())

;(defconstant violin-open-strings '((G-string 196)
;		       (D-string 293.66)
;		       (A-string 440)
;		       (E-string 659.25)))

;;;------------------------------------------------------------------------
;;;Overtone functions

;(defun overtone-ladder (fundamental gap n)
 ; (cond ((< n 1) nil)
;	(t (cons (+ fundamental gap)
;		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

;(defun overtones (fundamental)
 ; (cons fundamental
;	(overtone-ladder fundamental fundamental 15)))


(defun open-string-set (open-string-list)
  (cond ((null open-string-list) nil)
	(t (cons (overtones (first open-string-list))
		 (open-string-set (rest open-string-list))))))

(defvar open-string-overtones (open-string-set open-strings))

;;;;------------------------------------------------------------------------
;;;Resonance functions

;(defun resonance-compare (note open-string)
 ; (loop for overtone in (overtones note)
;	do (loop for ovtone in (overtones open-string)
;		 if (< (abs (- overtone  ovtone)) 10)
;		   collect ovtone into common-overtones
;		 finally (return common-overtones))))

(defun resonance-check (pitch string-overtones)
  (loop for overtone in string-overtones
	if (< (abs (- overtone pitch)) 10)
	  collect  overtone into common-overtones
	finally (return common-overtones)))

(defun resonance-compare (note-overtones open-string)
  (cond ((null note-overtones) nil)
	((resonance-check (first note-overtones)
			  (overtones open-string))
	 (cons (resonance-check (first note-overtones)
				  (overtones open-string))
	       (resonance-compare (rest note-overtones) open-string)))
	(t (resonance-compare (rest note-overtones) open-string))))
	
;;;------------------------------------------------------------------------
;;;Sympathetic Vibration functions:

(defun open-sympathizer (note string-list)
  (cond ((null string-list) nil)
	(t (cons (cons (first (first string-list))
		       (resonance-compare (overtones note)
					  (second (first string-list))))
		 (open-sympathizer note (rest string-list))))))

(defun sympathy-calculator (note string-list)
  (cond ((null string-list) nil)
	(t (append (resonance-compare (overtones note)
				    (first string-list))
		 (sympathy-calculator note (rest string-list))))))

(defun how-sympathetic (note)
  (length (sympathy-calculator note open-strings)))

;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------


;;;;Next steps- Given pitch as input, provide output in form

;given note in form (letter octave)
;(A 3) ->
;returns:
;1. Sympathetic Rating
;2. A list of resonant frequencies on each string
;3. Ideally whether to play slightly sharper or slightly flatter

;(defun resonance-calculator (note-name octave))

;;;;note-freq.lisp
;(defun note-to-freq (note-name octave);;;has to use quoted note-name
 ; (freq-finder (rest (assoc note-name note-freq-table)) octave))

;(defun freq-finder (note-freq octaves-up)
 ; (cond ((zerop octaves-up) note-freq)
;	(t (freq-finder (* 2 note-freq) (- octaves-up 1)))))

;(defun minimize-freq (freq)
 ; (cond ((< freq 31) freq)
;	(t (minimize-freq (/ freq 2)))))

;(defun freq-compare)

;(defun freq-to-note (freq)
 ; (loop for i in note-freq-table
  ;      collect
	;;;;find the minimum abs difference between freq and i
   
;(defvar note-freq-table '((C . 16.35)
;			  (C# . 17.32)
;			  ;(Db . 17.32)
;			  (D . 18.35)
;			  (D# . 19.45)
;			  ;(Eb . 19.45)
;			  (E . 20.6)
;			  (F . 21.83)
;			  (F# . 23.12)
;			  ;(Gb . 23.12)
;			  (G . 24.5)
;			  (G# . 25.96)
;			  ;(Ab . 25.96)
;;			  (A . 27.5)
;			  (Bb . 29.14)
;			  (B . 30.87)))

;(defvar freq-key '())

;(defun freq-to-note (freq)
 ; (setq freq-key '(c 20))
 ; (closest-note (minimize-freq freq) note-freq-table))

;(defun closest-note (freq freq-list)
 ; (cond ((null freq-list) (first freq-key))
;	((< (abs (- freq (rest (first freq-list))))
;	    (second freq-key))
;	 (progn (setq freq-key
;		      (list (first (first freq-list))
;			    (abs (- freq (rest (first freq-list))))))
;		(closest-note freq (rest freq-list))))
;	(t (closest-note freq (rest freq-list)))))


(defun keyed-overtone-ladder (fundamental gap n)
  (cond ((< n 1) nil)
	(t (cons (list (freq-to-note (+ fundamental gap))
		       (+ fundamental gap))
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(defun keyed-overtones (fundamental)
  (cons (list (freq-to-note fundamental) fundamental)
	(overtone-ladder fundamental fundamental 15)))
    
(defun keyed-resonance-check (pitch string-overtones)
  (loop for overtone in string-overtones
	if (< (abs (- overtone pitch)) 10)
	  do (return (list (freq-to-note overtone)
			overtone))))

(defun keyed-resonance-compare (note-overtones open-string)
  (cond ((null note-overtones) nil)
	((keyed-resonance-check (first note-overtones)
			  (overtones open-string))
	 (cons (keyed-resonance-check (first note-overtones)
				  (overtones open-string))
	       (keyed-resonance-compare (rest note-overtones) open-string)))
	(t (keyed-resonance-compare (rest note-overtones) open-string))))

(defun keyed-open-sympathizer (note string-list)
  (cond ((null string-list) nil)
	(t (cons (cons (first (first string-list))
		       (keyed-resonance-compare (overtones note)
					  (second (first string-list))))
		 (keyed-open-sympathizer note (rest string-list))))))

(defun keyed-sympathy-calculator (note string-list)
  (cond ((null string-list) nil)
	(t (append (list (first (first string-list))
			 (keyed-resonance-compare (overtones note)
			   (second (first string-list))))
		   (keyed-sympathy-calculator note (rest string-list))))))

(defun symp-rating (note-name octave)
  (length (sympathy-calculator (note-to-freq note-name octave) open-strings)))

(defun violin-resonance-calculator (note-name octave)
  (keyed-sympathy-calculator (note-to-freq note-name octave)
			     violin-open-strings))

(defun violin (note-name octave) ;(violin 'a 3)
  (keyed-sympathy-calculator (note-to-freq note-name octave)
			     violin-open-strings))

(defun violin (note-name octave &optional string-name)
  ;;;;skip the string it's played on
  )
;;;;------------------------------------------------------------------------
;;;;examples.lisp
;;;;examples using resound
;;;;1. find most resonant note on (insert instrument)
;;;;2.generate a
;;;;3. analyze resonance of scale/piece
;;;;4. 

;;;;------------------------------------------------------------------------
;;;;------------------------------------------------------------------------
;;;;------------------------------------------------------------------------
;;;;Calculate most resonant note on the violin
;(defvar most-resonant-note '(10 (note octave)))

;(defvar notes '(C C# D D# E F F# G G# A Bb B))

;(defvar octaves '(3 4 5 6 7 8 9))

;(defun most-resonant-check (note-list octave-list)
 ; (cond ((null octave-list) most-resonant-note)
;	((null note-list) (most-resonant-check notes (rest octave-list)))
;	((> (symp-rating (first note-list) (first octave-list))
;	    (first most-resonant-note))
;	 (progn (setq most-resonant-note '((symp-rating (first note-list)
;					    (first octave-list))
;					   (first note-list)
;					   (first octave-list)))
;		(most-resonant-check (rest note-list) octave-list)))
;	(t (most-resonant-check (rest note-list) octave-list))))

;)))))))))
;(defun most-resonant ()
 ;  (setq most-resonant-note '(1 (note octave)))
  ; (most-resonant-check (member 'g notes) octaves))
					   
  ;;;;loop for notes and octaves
  ;;;;collect (symp-rating, note octave)
  ;;;;do reduce'#lambda something to find the max
  ;;;;of sympratings, return note/octave

  ;;;(rest (assoc (max (mapcar #'first (loop for notes and octaves))
					; (loop for notes and octaves)


;(defun most-resonant-loop (note-list octave-list)
 ; (loop for octave in octave-list
;	do (loop for note in note-list
;		collect (list note octave))))



       
;;;;--------------------------------------
;;;;Web app

	  ;;Input: Note/Octave

;;;;output: Note/Octave
	   ;G String: ...
           ;D string: ...

;;;optimal

;(defun optimal-resonant-note ())
;;;go through all notes g 3 -


;;;;scheme/lilypond extension that turns notes gray if they're resonant
;-should be pretty friendly 


;;;A4 played on the G string may be the most resonant note on the violin
