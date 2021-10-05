;;;;resonance.lisp

(in-package #:resound)

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



(defun compare-overtones (fund1 fund2)
  "Compares the overtones of two fundamentals, returns overlap."
  
   (overtones fund1) (overtones fund2))
;;;; take a list of overtones, 

(defun common-overtones-backend (overtones1 overtones2)
  (cond ((null overtones1) nil)
	(t (union (loop for i in overtones2
		       if (< (abs (- i (first overtones1))))
			 collect i into common-overtones
		       finally (return common-overtones))
		 (common-overtones-backend
		  (rest overtones1) overtones2)))))
;	((< (abs (- (first overtones1) (first overtones2))) 10)
    
   
(defun resonance-compare (overtones1 overtones2)
  (cond ((null overtones1) nil)
	((resonance-check (first note-overtonesovertones)
			  string-overtones)
	 (cons (resonance-check (first note-overtones)
				  string-overtones)
	       (resonance-compare (rest note-overtones) string-overtones)))
	(t (resonance-compare (rest note-overtones) string-overtones))))










