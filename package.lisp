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
   #:make-note)
  
  ;;;;resonance.lisp
  (:export
   #:overtones
   #:compare-overtones) ;;;overtone functions, resonance check functions, but more basic

  ;;;;instruments.lisp
  (:export
   #:luthier)

  ;;;;analysis.lisp
  (:export
   #:symp-rating
   #:symp-rating-by-note
   #:symp-by-string
   #:assess-note))
