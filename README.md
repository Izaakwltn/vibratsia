# Vibratsia

#### An interface for calculating sympathetic vibrations between strings on stringed instruments.

Vibratsia is an attempt to analyze and harness these sympathetic vibrations for the purposes
of optimal sound generation, composing towards the instruments' natural strengths, and
understanding intonation.

## A quick dive into the science behind resonance:

Every note played on an instrument, or even sung, is actually a composite sound consisting
of approximately 32 overtones. On stringed instruments, these overtones can be isolated
using harmonics.

Sympathetic vibration occurs when the overtones of an executed note overlap with the
overtones of an open string. Harmonic nodes, as the overtones are called in string
geography, respond to similar frequencies, and vibrate the open string audibly,
and sometimes even visually. 

## Using Vibratsia

Since most analysis will be in the context of a musical instrument, there are a number of preset
instruments including the violin, viola, cello, bass, and hardanger fiddle.

You can also create your own instrument:

VIBRATSIA> (luthier 'violin violin-open-strings)

#<INSTRUMENT VIOLIN, strings: (#<NOTE G-3, Frequency: 196.0>
                               #<NOTE D-4, Frequency: 293.66>
                               #<NOTE A-4, Frequency: 440.0>
                               #<NOTE E-5, Frequency: 659.25>)> 
                               
Once you have an instrument configured, you can compare a note with it using note-name/octave syntax:
#### Sample note-assessment

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

                               
                               
