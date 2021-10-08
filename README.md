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

                               
                               
