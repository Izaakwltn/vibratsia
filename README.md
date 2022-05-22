# Vibratsia

#### An interface for calculating sympathetic vibrations between strings on stringed instruments.

Vibratsia is an attempt to analyze and harness these sympathetic vibrations for the purposes
of optimal sound generation, composing towards the instruments' natural strengths, and
understanding intonation.

## To use a web-app version: 
(ql:quickload :resonance-calculator) -> (resonance-calculator:launch)

# A quick dive into the science behind resonance:

Every note played on an instrument, or even sung, is actually a composite sound consisting
of approximately 32 overtones. On stringed instruments, these overtones can be isolated
using harmonics.

Sympathetic vibration occurs when the overtones of an executed note overlap with the
overtones of an open string. Harmonic nodes, as the overtones are called in string
geography, respond to similar frequencies, and vibrate the open string audibly,
and sometimes even visually. 

## Using Vibratsia in the REPL:

# Since most analysis will be in the context of a musical instrument, there are a number of preset instruments including the violin, viola, cello, bass, and hardanger fiddle.

# You can also create a custom instrument:

VIBRATSIA> (luthier 'violin violin-open-strings)

#<INSTRUMENT VIOLIN, strings: (#<NOTE G-3, Frequency: 196.0>
                               #<NOTE D-4, Frequency: 293.66>
                               #<NOTE A-4, Frequency: 440.0>
                               #<NOTE E-5, Frequency: 659.25>)> 
                               
# Once you have an instrument configured, you can compare a note with it using note-name/octave syntax:

 VIBRATSIA> (assess-note 'a 4 violin)

#<NOTE-ASSESSMENT 
The resonant profile of:

#<NOTE A-4, Frequency: 440.0>
                          
as played on the: 
#<INSTRUMENT VIOLIN, strings: (#<NOTE G-3, Frequency: 196.0>
                               #<NOTE D-4, Frequency: 293.66>
                               #<NOTE A-4, Frequency: 440.0>
                               #<NOTE E-5, Frequency: 659.25>)>

                          
Sympathetic Vibration Rating: 27, which is highly resonant.

                          
A list of Frequencies by String:

(G STRING (#<NOTE A-6, Frequency: 1764.0>) D STRING
 (#<NOTE A-5, Frequency: 880.98> #<NOTE A-6, Frequency: 1761.9601>
  #<NOTE E-7, Frequency: 2642.94> #<NOTE A-7, Frequency: 3523.9197>
  #<NOTE C#-8, Frequency: 4404.9>)
 A STRING
 (#<NOTE A-4, Frequency: 440.0> #<NOTE A-5, Frequency: 880.0>
  #<NOTE E-6, Frequency: 1320.0> #<NOTE A-6, Frequency: 1760.0>
  #<NOTE C#-7, Frequency: 2200.0> #<NOTE E-7, Frequency: 2640.0>
  #<NOTE G-7, Frequency: 3080.0> #<NOTE A-7, Frequency: 3520.0>
  #<NOTE B-7, Frequency: 3960.0> #<NOTE C#-8, Frequency: 4400.0>
  #<NOTE D#-8, Frequency: 4840.0> #<NOTE E-8, Frequency: 5280.0>
  #<NOTE F-8, Frequency: 5720.0> #<NOTE G-8, Frequency: 6160.0>
  #<NOTE G#-8, Frequency: 6600.0> #<NOTE A-8, Frequency: 7040.0>)
 E STRING
 (#<NOTE E-6, Frequency: 1318.5> #<NOTE E-7, Frequency: 2637.0>
  #<NOTE B-7, Frequency: 3955.5> #<NOTE E-8, Frequency: 5274.0>
  #<NOTE G#-8, Frequency: 6592.5>))>

# And you can see the most resonant notes on an instrument using (assess-instrument)

VIBRATSIA> (assess-instrument violin)

#<INSTRUMENT-ASSESSMENT 
                        
#<INSTRUMENT VIOLIN, strings: (#<NOTE G-3, Frequency: 196.0>
                               #<NOTE D-4, Frequency: 293.66>
                               #<NOTE A-4, Frequency: 440.0>
                               #<NOTE E-5, Frequency: 659.25>)>

Note Ranking by Number of Sympathetic Vibrations:

 (27 #<NOTE D-4, Frequency: 293.66827>)
(27 #<NOTE A-4, Frequency: 440.0054>)
(22 #<NOTE E-5, Frequency: 659.2635>)
(18 #<NOTE A-3, Frequency: 220.00258>)
(17 #<NOTE D-5, Frequency: 587.3369>)
(17 #<NOTE A-5, Frequency: 880.0114>)
(14 #<NOTE E-4, Frequency: 329.63153>)
(14 #<NOTE E-6, Frequency: 1318.5277>)
(13 #<NOTE G-4, Frequency: 392.0002>)
(8 #<NOTE D-6, Frequency: 1174.6746>)
(8 #<NOTE A-6, Frequency: 1760.0237>)
(6 #<NOTE B-4, Frequency: 493.8894>)
(6 #<NOTE G-5, Frequency: 784.001>)
(6 #<NOTE B-5, Frequency: 987.7795>)
(5 #<NOTE C-4, Frequency: 261.62866>)
(5 #<NOTE B-6, Frequency: 1975.5599>)
(4 #<NOTE B-3, Frequency: 246.94456>)
(3 #<NOTE G-6, Frequency: 1568.0027>)
(2 #<NOTE C-5, Frequency: 523.2576>)
(1 #<NOTE BB-3, Frequency: 233.08463>)
(1 #<NOTE F-4, Frequency: 349.23245>)
(1 #<NOTE C-6, Frequency: 1046.516>)
(0 #<NOTE G#-3, Frequency: 207.65477>)
(0 #<NOTE C#-4, Frequency: 277.1859>)
(0 #<NOTE D#-4, Frequency: 311.1307>)
(0 #<NOTE F#-4, Frequency: 369.9989>)
(0 #<NOTE G#-4, Frequency: 415.30978>)
(0 #<NOTE BB-4, Frequency: 466.1695>)
(0 #<NOTE C#-5, Frequency: 554.3722>)
(0 #<NOTE D#-5, Frequency: 622.26184>)
(0 #<NOTE F-5, Frequency: 698.4654>)
(0 #<NOTE F#-5, Frequency: 739.99835>)
(0 #<NOTE G#-5, Frequency: 830.6201>)
(0 #<NOTE BB-5, Frequency: 932.33966>)
(0 #<NOTE C#-6, Frequency: 1108.7451>)
(0 #<NOTE D#-6, Frequency: 1244.5244>)
(0 #<NOTE F-6, Frequency: 1396.9315>)
(0 #<NOTE F#-6, Frequency: 1479.9974>)
(0 #<NOTE G#-6, Frequency: 1661.2411>)
(0 #<NOTE BB-6, Frequency: 1864.6802>)
(0 #<NOTE C-7, Frequency: 2093.033>)
>
                               
                               
