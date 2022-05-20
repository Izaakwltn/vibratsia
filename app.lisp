;;;; resonance-calculator/app.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :resonance-calculator)

;;; Generating menu option lists: 

(defvar note-option-list (mapcar #'vibratsia::freq-to-note
			     (vibratsia::frequency-ladder 16.35 4185.6)))

(defvar instrument-option-list '("violin" "viola" "cello" "bass" "hardingfele"))
                                        ;add more maybe

(defvar instrument-parse-list '(("violin" vibratsia:violin)
                                ("viola" vibratsia:viola)
                                ("cello" vibratsia:cello)
                                ("bass" vibratsia:bass)
                                ("hardingfele" vibratsia:hardanger-fiddle)))

;(defvar instrument-parse-list '(("violin"
;				 (vibratsia::luthier 'violin '(196 293.66 440 659.25)))
;				 ("VIOLA"
;				  (vibratsia::luthier 'viola '(130.8 196 293.66 440)))
;				("CELLO"
;				 (vibratsia::luthier 'cello '(65.4 98 146.8  220)))
;				("BASS"
;				 (vibratsia::luthier 'bass '(65.4 98 146.8  220)))
;				("hardingfele"
;				 (vibratsia::luthier 'hardanger-fiddle
;				  vibratsia::hardanger-fiddle-strings))))
				 

(defvar root-option-list '(C C# D D# E F F# G G# A Bb B))

(defvar root-parse-list '(("c" vibratsia::c)
			  ("c#" vibratsia::c#)
			  ("d" vibratsia::d)
			  ("d#" vibratsia::d#)
			  ("e" vibratsia::e)
			  ("f" vibratsia::f)
                          ("f#" vibratsia::f#)
			  ("g" vibratsia::g)
			  ("g#" vibratsia::g#)
			  ("a" vibratsia::a)
			  ("bb" vibratsia::bb)
			  ("b" vibratsia::b)))

(defvar quality-parse-list '(("major" vibratsia::major)
			     ("ionian" vibratsia::ionian)
			     ( "dorian" vibratsia::dorian)
			     ("phrygian" vibratsia::phrygian)
			     ("lydian"   vibratsia::lydian)
			     ("mixolydian" vibratsia::mixolydian)
			     ("aeolian" vibratsia::aeolian)
			     ("nat-min" vibratsia::nat-min)
			     ("mel-min" vibratsia::mel-min)
			     ("har-min" vibratsia::har-min)
			     ("locrian" vibratsia::locrian)))

(defvar quality-option-list
  '(major ionian dorian phrygian lydian mixolydian aeolian nat-min mel-min har-min locrian))

;;;;------------------------------------------------------------------------

(defun resonance ()
  (with-page (:title "Resonance Calculator")
    (:header
     (:h1 "Resonance Calculator"))
    (:section
     (:p "Every note played on an instrument, or even sung, is actually a composite sound consisting of approximately 32 overtones. On stringed instruments, these overtones can be isolated using harmonics.

Sympathetic vibration occurs when the overtones of an executed note overlap with the overtones of an open string. Harmonic nodes, as the overtones are called in string geography, respond to similar frequencies, and vibrate the open string audibly, and sometimes even visually."))
    (:section
     (:h2 "Instrument Assessment")
     (:p "Select an instrument to calculate its most resonant keys and most resonant notes:")
     (:form :action "/instrument-assess" :id "instrument-assess"
      (:select :name "instrument-option" :form "instrument-assess"
        (loop for instrument in resonance-calculator::instrument-option-list
	      do (:option :value instrument (format nil "~a" instrument))))
      (:input :type "submit" :value "Assess Instrument" :class "button")))
    (:section
     (:h3 (:i "Key Assessment"))
     (:p "Select the tonic, scale quality, and instrument to find the most resonant notes in the key.")
     (:form :action "/key-assess" :id "key-assess"
      (:select :name "root-option" :form "key-assess"
       (loop for r in resonance-calculator::root-option-list
	     do (:option :value r (format nil "~A" r))))
      (:select :name "quality-option" :form "key-assess"
       (loop for qual in resonance-calculator::quality-option-list
	      do (:option :value qual (format nil "~a" qual))))
      (:select :name "instrument-option" :form "key-assess"
        (loop for instrument in resonance-calculator::instrument-option-list
	      do (:option :value instrument (format nil "~a" instrument))))
      (:input :type "submit" :value "Assess Key" :class "button")))
    (:section
     (:h2 "Note Assessment")
     (:p "Select a note and an instrument to calculate the sympathetic frequencies on each string.")
     (:form :action "/note-assess" :id "note-assess"
            :method "get"
      (:select :name "note-option" :form "note-assess"
	(loop for note in (vibratsia::frequency-ladder 16.35 4185.6)
	      do (:option :value note
			  (format nil "~a-~a"
				  (first (freq-to-note note))
					 (second (freq-to-note note))))))
      (:select :name "instrument-option" :form "note-assess"
	(loop for instrument in resonance-calculator::instrument-option-list
	      do (:option :value instrument
			  (format nil "~a" instrument))))
      (:input :type "submit" :value "Assess Note" :class "button" )))))

(push (hunchentoot::create-prefix-dispatcher "/resonance.html" #'resonance)
      hunchentoot::*dispatch-table*)
