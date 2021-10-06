;;;;package.lisp

(defpackage #:vibratsia
  (:use #:cl)

  ;;;;pitch.lisp
  (:export
   #:note-to-freq
   #:freq-to-note
   #:note
   #:make-note)
  
  ;;;;resonance.lisp
  (:export
   #:overtones
   #:compare-overtones) ;;;overtone functions, resonance check functions, but more basic

  ;;;;instruments.lisp
  (:export
   #:instrument
   #:luthier)

  ;;;;analysis.lisp

;etc
(:documentation "Resonance Calculation Package."))
