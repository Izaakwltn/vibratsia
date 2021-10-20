;;;;package.lisp
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(defpackage #:vibratsia
  (:documentation "String Resonance Calculation Package.")
  (:use #:cl #:alexandria)

  ;;;;pitch.lisp
  (:export
   #:note-to-freq
   #:freq-to-note
   #:make-note
   #:freq-adjust
   #:freq-incr
   #:frequency-ladder)
  
  ;;;;resonance.lisp
  (:export
   #:overtones
   #:compare-overtones) ;;;overtone functions, resonance check functions, but more basic

  ;;;;instruments.lisp
  (:export
   #:luthier)

  ;;;;scales.lisp
  (:export
   #:build-scale
   #:scale-ranking
   #:assess-scale)
   
  ;;;;analysis.lisp
  (:export
   #:symp-rating
   #:symp-rating-by-note
   #:symp-by-string
   #:assess-note
   #:most-resonant
   #:resonance-ranking
   #:assess-instrument
   #:avg-resonance))
