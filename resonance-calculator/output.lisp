;;;; resonance-calculator/resonance-output.lisp
;;;;
;;;; Copyright (c) 2021-2022 Izaak Walton

(in-package #:resonance-calculator)

;;;;------------------------------------------------------------------------
;;;;Instrument Assessment
;;;;------------------------------------------------------------------------

(defvar instrument-parse-list '(("violin"
				 (vibratsia::luthier 'violin '(196 293.66 440 659.25)))
				 ("VIOLA"
				  (vibratsia::luthier 'viola '(130.8 196 293.66 440)))
				("CELLO"
				 (vibratsia::luthier 'cello '(65.4 98 146.8  220)))
				("BASS"
				 (vibratsia::luthier 'bass '(65.4 98 146.8  220)))))

(defvar instr-assessment)

(hunchentoot::define-easy-handler (instrument-assess :uri "/instrument-assess")
    (instrument-option)
  (setf (hunchentoot:content-type*) "text/html")
  (setq instr-assessment (vibratsia::assess-instrument
	   (eval (second (assoc instrument-option instrument-parse-list :test #'string-equal)))))
  (with-page (:title "Instrument Resonance Profile")
    (:header
     (:h1 "Resonance Calculator")
     (:h2 "Instrument Assessment")
     (:p "A comprehensive analysis of the resonant keys and frequencies on the violin. "))
    (:section
     (:h5 (format nil "Instrument name: ~a"
		  (vibratsia::name (vibratsia::instrument instr-assessment)))))
    (:section
     (:h5 (format nil "Open-strings:~%"))
     (:ul (loop for s in (vibratsia::strings (vibratsia::instrument instr-assessment))
		do (:li (format nil "~a-~a Frequency: ~a"
			   (vibratsia::note-name s)
			   (vibratsia::octave s)
			   (vibratsia::freq-float s))))))
    (:section
     (:h5 (format nil "Most Resonant Keys:"))
     (:ul (loop for k in (vibratsia::key-ranks instr-assessment)
		do (:li (format nil "~a ~a, resonance rating: ~a"
				(second k)
				(third k)
				(first k))))))
    (:section
     (:h5 (format nil "Most Resonant Notes:"))
     (:ul (loop for n in (vibratsia::note-ranks instr-assessment)
		do (:li (format nil "~a-~a Frequency: ~a"
				(vibratsia::note-name (second n))
				(vibratsia::octave (second n))
				(vibratsia::freq-float (second n)))))))
	       (:a :href "/resonance.html" "Try another calculation")))


;;;;------------------------------------------------------------------------
;;;;Key Assessment
;;;;------------------------------------------------------------------------
;(vibratsia::assess-scale (vibratsia::build-scale root-option quality-option ;3) instrument-option)

					;(defvar quality-parse-list '(("major"
(defun lowest-note-available (note-name instrument)
  "Finds the lowest octave of a particular note on an instrument"
  (second (assoc (second (assoc note-name root-parse-list :test #'string-equal))
  (mapcar #'(lambda (freq)
			       (list (first (vibratsia::freq-to-note freq))
				     freq))
	  (vibratsia::frequency-ladder
	   (vibratsia::lower-bound instrument)
	   (vibratsia::freq-adjust (vibratsia::lower-bound instrument) 11))))))

(hunchentoot::define-easy-handler (scale-assess :uri "/key-assess")
	(root-option quality-option instrument-option)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((instrument-object (eval
                            (second
                             (assoc instrument-option
                                    instrument-parse-list :test #'string-equal))))
        (key-assessment (vibratsia::assess-scale
                         (vibratsia::build-scale
			  (lowest-note-available root-option instrument-object)
                          (second (assoc quality-option quality-parse-list :test #'string-equal))
			  3)
                         instrument-object)))
    (with-page (:title "Key Resonance Profile")
      (:header
       (:h1 "Resonance Calculator")
       (:h2 "Key Resonance Profile")
       (:p (format nil "A comprehensive analysis of the key ~a ~a, as played on the ~a"
                   (first (vibratsia::freq-to-note
                           (vibratsia::root (vibratsia::scale key-assessment))))
		 (vibratsia::quality (vibratsia::scale key-assessment))
		 (vibratsia::name (vibratsia::instr key-assessment)))))
      (:section
       (:h5 (format nil "Average Sympathetic Vibration Rating:  ~a"
		  (vibratsia::avg-rating key-assessment))))
    (:section
     (:h5 "The notes of the key ranked by resonance on the chosen instrument:")
     (:ul
      (loop for n in (vibratsia::rank-list key-assessment)
	    do (:li (format nil "~a" n)))))
	       (:a :href "/resonance.html" "Try another calculation"))))
     

;;;;find the lowest iteration of the key on the instrument, generate a 3 octave scale, return
					;the scale assessment

;;;;------------------------------------------------------------------------
;;;;Note Assessment
;;;;------------------------------------------------------------------------

(defvar note-assessment)

(hunchentoot::define-easy-handler (note-assess :uri "/note-assess") (note-option instrument-option)
  (setf (hunchentoot:content-type*) "text/html")
  (setq note-assessment
	(let ((note (vibratsia::make-note (parse-float:parse-float note-option))))
          (vibratsia::assess-note (eval (second (assoc instrument-option
					      instrument-parse-list :test #'string-equal)))
                                  note)))
  (with-page (:title "Instrument Resonance Profile")
    (:header
     (:h1 "Resonance Calculator")
     (:h2 "Note-instrument Assessment")
     (:p (format nil "A comprehensive analysis of the note, ~a-~a, as played on the ~a"
		 (first (vibratsia::freq-to-note (parse-float:parse-float note-option)))
		 (second (vibratsia::freq-to-note (parse-float:parse-float note-option)))
		 instrument-option)))
    (:section
     (:h5 (format nil "Sympathetic Vibration Rating: ~a" (vibratsia::rating note-assessment))))
    (:section
     (:h5 "List of Resonant Frequencies:")
     (:ul
      (loop for note in (vibratsia::res-list note-assessment)
	    if (equal (second note) 'string)
	      do (:li (:b (format nil "~a ~a" (first note) (second note))))
	    else
	      do (:ul
		  (loop for n in note
			do (:li (format nil "~a" n)))))))
	       (:a :href "/resonance.html" "Try another calculation")))
