;;;;package.lisp

(defpackage #:vibratsia
  (:use #:cl)

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
   #:assess-note

;etc
(:documentation "Resonance Calculation Package."))
