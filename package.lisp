;;;;package.lisp

(defpackage #:resound
  (:use #:cl)

  ;;;;pitch.lisp
  (:export
   #:note-to-freq
   #:freq-to-note
   #:make-note)
  
  ;;;;resonance.lisp
  (:export
   #:overtones
   ) ;;;overtone functions, resonance check functions, but more basic

  ;;;;instruments.lisp
  ;(:export)

  ;;;;pitch.lisp
  ;(:export)

  ;;;;analysis.lisp

;etc
(:documentation "Resonance Calculation Package."))
