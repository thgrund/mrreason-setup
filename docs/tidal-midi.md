# TidalMIDI

This project allows to create midi files based on TidalCycles patterns. You need to import it with `import Sound.Tidal.MIDI` first.

## Requirements

You simply need to install this corresponding cabal package with `cabal v2-install --lib`

## How To use it

First note based pattern with default settings:

```haskell
 toFile "/path/to/file/name.mid" (midiFile midiScore
   [
     midiNoteTrack AcousticGrandPiano (s "superpiano" <| note "[c6'maj7, c3 e3 g3 b3]") midiScore
   ])

```

Here we set the instrument to `AcoustigGrandPiano` . A list of all available instruments can be found when executing `instruments` form `Sound.MIDI` .
This will create a midi file with 120 bpm and 4/4 time signature.

To create a midi track with drums you can use this:

```haskell
toFile "/path/to/file/name.mid" (midiFile midiScore [
     midiDrumTrack (s "[bd(3,8), cr, hh*8, ~ sn]") midiScore
   ])
```

This will map the sample names to general midi drum values.

To change the bpm and add time signatures to your midi file, you need to adjust the `midiScore` record like this:

```haskell
do
let midiScore' = midiScore {bpm = 138
	  , timeSignatures = [(1 % 1, 3, 2),(2 % 1, 7, 2), (3 % 1, 9 , 3), (4 % 1, 5 , 2)]}
toFile "/Users/mrreason/Documents/MidiExample/1.mid" (midiFile midiScore' [
    midiNoteTrack ElectricGuitarClean
    	(struct "<[t*3] ~! [t*5] >" $  s "piano" <| note "<a b c d e f>") midiScore'
  ] )
```

This will change the `bpm` to 138 and add the time signature as followed:

- Till the end of bar 1: 6/8 time signature
- Till the end of bar 2: 7/4 time signature
- Till the end of bar 3: 9/8 time signature
- Till the end of bar 4: 5/4 time signature

To prove that everything is calculated correctly, the second and third cycle is a rest.

The current `midiScore` definition looks like this:

```  haskell
midiScore = TidalMIDIScore {
  timeSignatures = [(1 % 1, 4, 2)],
  bpm = 120,
  duration = 4
}
```

The time signature calculation is implemented by the MIDI standard.

To create a larger midi file you need to adapt the `duration` value as well.
