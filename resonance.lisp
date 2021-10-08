;;;;resonance.lisp

(in-package #:vibratsia)

;;;;------------------------------------------------------------------------
;;;;Overtone Functions
;;;;------------------------------------------------------------------------

(defun overtone-ladder (fundamental gap n)
  (cond ((< n 1) nil)
	(t (cons (+ fundamental gap)
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(defun overtones (fundamental)
  "Generates a list of overtone frequencies for a given pitch-frequency."
  (cons fundamental
	(overtone-ladder fundamental fundamental 15)))

;;;;------------------------------------------------------------------------
;;;;Resonance Functions
;;;;------------------------------------------------------------------------

(defun resonance-check (pitch overtone-list)
  (loop for overtone in overtone-list
	if (< (abs (- overtone pitch)) 10)
	  collect  overtone into common-overtones
	finally (return common-overtones)))

(defun resonance-compare (overtones1 overtones2)
  (cond ((null overtones1) nil)
	((resonance-check (first overtones1) overtones2)
	 (append (resonance-check (first overtones1) overtones2)
	       (resonance-compare (rest overtones1) overtones2)))
	(t (resonance-compare (rest overtones1) overtones2))))

(defun compare-overtones (fund1 fund2)
  "Compares the overtones of two fundamentals, returns sympathetic overlap."
  (resonance-compare (overtones fund1) (overtones fund2)))


;;;;------------------------------------------------------------------------
;;;;Maybe sympathetic 
;;;;------------------------------------------------------------------------
