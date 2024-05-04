# MrReason Setup
Design, ideas and motivation for my TidalCycles setup.

I have divided this post into different sections.
Here it is about quite different aspects like among others the live coding of a larger set with multiple parts and transitions (live coding paired with code DJing) or the application of western music theory.

To be able to use the described concepts, the code under `./MrReason.hs` should be executed first. For this I load the file with `:l` in my `BootTidal.hs` like this: 
```haskell
:l /Users/mrreason/Documents/livecoding/tidal/mrreason-setup/MrReason.hs
```

Beside the concepts I describe here, I highly frequently use some other projects I created in combination with them: 

- [SuperDirtMixer](https://github.com/thgrund/SuperDirtMixer)
- [TidalLooper](https://github.com/thgrund/tidal-looper)
- [TidalVST](https://github.com/thgrund/TidalVST)
- [Tidal Pedalboard effects](https://github.com/thgrund/tidal-pedalboard-effects)

Table of content:

- [Applying modern western music theory](#applying-western-music-theory)
- [Custom conditional functions](#custom-conditional-functions)
- [Live coding with parts and segments (for longer compositions)](#live-coding-with-parts-and-segments)
- [Tips and hints for live coding with longer sets](#tips-and-hints-for-live-coding-with-longer-sets)

## Infrastructure overview

This is a screenshot from my editor and it shows what the audience sees. The informations I share are: 

- Artist name
- Song name
- Number of segments and active segment
- Audio visualizer
- Code (what you see is what you hear)

### ![Editor](./asset/images/Editor.png)

### Message flow between components

Uses partly non published code from https://github.com/thgrund/ep (private)

This is an overview to show, how every piece in my setup exchanges states and informations:

![MrReasonSetup.drawio](./asset/images/MrReasonSetup.drawio.svg)

### OSC Messages

#### SuperCollider (Remote.scd)

```c++
atomOSC.sendMsg("/pulsar/eval", "search", value.segment[0]);
pulsarOSC.sendMsg("/pulsar/gradient", "gradient", "orangered");

hydaOSC.sendMsg("/hydra/video", "source", videoSource);

tidalOSC.sendMsg("/ctrl", "duration", value.segment[2]);
tidalOSC.sendMsg("/ctrl", "offset", (cycle % value.segment[2]).neg + offset);
tidalOSC.sendMsg("/ctrl", parsedJSON.songName, value.segment[3]);
tidalOSC.sendMsg("/ctrl", "presetFile", "~");

superDirtOSC.sendMsg("/SuperDirtMixer/loadPreset", value.segment[4]);
superDirtOSC.sendMsg("/SuperDirtMixer/tidalvstPreset", \pliniRhytm, "/Users/mrreason/Development/SuperCollider/TidalVST/presets/pliniX/forestPliniRhytm.vstpreset");
superDirtOSC.sendMsg("/SuperDirtMixer/midiControlButton", num - 100);


```

#### TidalCycles

```c++
"/ctrl" "duration" cycleNumber
"/ctrl" "offset" offeset
"/ctrl" parsedJSON.songName pattern | ur pattern like "1@4 2@1 3@8"
"/ctrl" "presetFile" "~"
```

#### Tidalcycles Package

```c++
"/pulsar/buttons"  "enabled" size "color" hexColor
"/pulsar/button": buttonIndex
"/pulsar/headlines": "songTitle" songTitle
"/pulsar/audio": AudioDevice Name (MediaDevice HTML5 Name)
"/pulsar/gradient": gradientColor (orangered | classic)
"/pulsar/remote-control/index"
"/pulsar/remote-control/preset"
"/pulsar/eval" "search" search term
```

#### SuperDirtMixer

```c++
"/SuperDirtMixer/loadPreset": presetFile
"/SuperDirtMixer/pan": orbitIndex value
"/SuperDirtMixer/masterGain": orbitIndex value
"/SuperDirtMixer/reverb": orbitIndex value
"/SuperDirtMixer/tidalvstPreset": fxName presetPath
"/SuperDirtMixer/midiControlButton": midiControlButtonIndex
```

#### Hydra

```c++
'/hydra/video': video path (relativ)
```

## Remote.scd

Master gain Fader mit MIDI 

```c++
masterCC = MIDIFunc.cc({ |val, num, chan, src|

	if (num == 121 or: {num == 7 or: {num == 115}}, {

		~dirt.orbits.do({
			arg orbit;

			orbit.globalEffects.do({
				arg globalEffect, index;

				if (globalEffect.name == 'dirt_master_mix', {
					//TODO Logik
					globalEffect.synth.set(\globalMasterGain, ((val + 1).linexp(1, 127, 1,2) - 1))
				});
			});
		})
	})
});
```

## Applying western music theory

My motivation was to make some rules accessible via TidalCycles using the system of western music theory that I am familiar with. However, I attach great importance to defining and transforming chord progressions and notes with patterns, in order to be able to use the strengths of TidalCycles flexibly here. 

### How it works

#### The 'Sheet' datatype

I use this template based on the `Sheet` definition that you can find under  `./MrReason.hs` and passed it to my `BootTidal.hs` file:

```haskell
sheet = Sheet {
  key = "c",
  mode = "major",
  numerals = "1",
  functions = id
}
```

When the record `sheet` is available, you can just create a new one and change just the elements you need to change:

```haskell
s1 = sheet {numerals = "<1 1 4 5>"}
```

I will explain the usage of the sheet record below. The numerals field was changed to allow the usage of numerals identifier. You can think of it as a simple parser on top of the TidalCycles parser.

#### Chords and melodies

The basic concept here is writing chord progressions with roman numerals. The chord will be created on their elements like root (1), major/minor third (3) and perfect fifth (5).

Building diatonic chords and chord progressions in c major could be look like this:

```haskell
s1 = sheet {numerals = "<1 1 4 5>"}
d1 $ s "superpiano" <| prog s1 "[1,3,5]"
```

But you could for example simply create a chord progression based on sus2 chords in this way

```haskell
d1 $ s "superpiano" <| prog s1 "[1,2,5]"
```

And of course you can pattern this behaviour:

```haskell
d1 $ s "superpiano" <| prog s1 "[1,<3 2>,5]"
```

Seventh chords based on the roman numerals based on their scale degree:

```haskell
d1 $ s "superpiano" <| prog s1 "[1,3,5,7]"
```

And I really like that you can easily write melodies or sequences that are related to the key and the chord changes (playing with the changes) but the melody notes don't need to be the exact notes from the chords:

```haskell
d2 $ s "superpiano" <| prog s1 "1 5 7@2 3 6 8@2" |+ note 12
```

#### Same key - different mode

The melody doesn't need to use them same mode, key or numerals. One use case is to play a chord progression in major and use the minor pentatonic for the melody like this:

```haskell
do
let s1 = sheet {mode = "major", numerals = "<1 1 4 5>"}
    s2 = s1 {mode = "minPent" }
d1 $ s "superpiano" <| prog s1 "[1,3,5]"
d2 $ s "superpiano" <| prog s2 "1 2 3@2 2 3 4@2"
```

In this way you play you could say "on top of the changes" instead of "with the changes":

#### Changing the key

You can change the key of a performance or transpose it easily like:

```haskell
-- Default c
s1 = sheet {key = "c", numerals = "<1 1 4 5>"}

-- Transpose two half steps down
s1 = sheet {key = "c" |- 2, numerals = "<1 1 4 5>"}

-- Transponse the key with the note hight 
s1 = sheet {key = "c3", numerals = "<1 1 4 5>"}

-- A different note for a different key
s1 = sheet {key = "d", numerals = "<1 1 4 5>"}

-- Change the key in perfect fifths (segment of the circle of fifths:)
s1 = sheet {key = "<c g d a>", numerals = "<1>"}
```

#### Apply functions to the complete progression 

It's possible to apply functions to the complete progression. Unfortunately you can not directly apply them in `numerals`. For this I added a `functions` property to the `Sheet` datatype. This i.e. works: 

```haskell
s1 = sheet {numerals = "<1 1 4 5>", functions = fast 4}
```

And you can even compose multiple functions like this: 

```haskell
s1 = sheet {numerals = "<1 1 4 5>", functions = rev . (fast 4)}
```

If you don't specify the `functions` property, then the identity function `id` will be used. This means no change to the original chord progression will be applied.

### Numerals Identifier

With a numerals identifier you are able to specifically manipulate the result of a numerals occurrence. This is a nice shorthand to i.e. change scales or to increase/decrease the value chromatically stepwise. An identifier is always linked to a numerals and will be introduced by a `#` sign:   

```haskell
s1 = sheet {numerals = "1#+"}
```

You can combine multiple identifiers:

```haskell
s1 = sheet {numerals = "1#A+"}
```

You need to treat the combination of numbers and identfiers as one construct. You are not allowed to pattern only one part. If you want to do this, then you need to separate it like this: 

```haskell
s1 = sheet {numerals = "<<1#+ 1#--> 5>"}
```

This is a list of all identifiers that exist yet:

| Identifier | Description                                                  |
| ---------- | ------------------------------------------------------------ |
| +          | Increment the numerals chromatically                         |
| -          | Decrement the numerals chromatically                         |
| O          | Increase the numerals by an octave                           |
| o          | Decrease the numerals by an octave                           |
| T          | Increase the numerals by a tritone.                          |
| t          | Decrease the numerals by a tritone.                          |
| S          | Shifts the numerals as it would 5 of the scale. Useful to create secondary dominants. The result will be above the underlying numerals |
| s          | Like the `S` modifier but lowered by 12 semitones            |
| d          | Uses a diminished scale at the specific position             |
| a          | Uses an augmented scale at the specific position             |
| I          | Uses an ionian/major scale at the specific position          |
| D          | Uses a dorian scale at the specific position                 |
| P          | Uses a phrygian scale at the specific position               |
| L          | Uses a lydian scale at the specific position                 |
| M          | Uses a mixolydian scale at the specific position             |
| A          | Uses an aeolian scale at the specific position               |

### Borrowing Chords

When you want to borrow a chord explicitly from an other key, then you simply change the mode

```haskell
s1 = sheet {mode = "<phrygian!3 minor>", numerals = "<1 2 3 5>"}
```

The same is achievable with an identifier. I.e. you can use the phrygian mode as a default mode and replace the last value in numerals with the aeolian mode (means minor): 

```haskell
s1 = sheet {mode = "phrygian", numerals = "<1 2 3 5#A>"}
```

### Secondary dominants

The secondary dominant will be played 5 half steps lower then the original chord would be. This is simply a major chord with a (possible) dominant seven chord (V7) 7 half steps above the original chord. To use it use the `#s` identifier in numerals on the value you want to apply this.

```haskell
s1 = sheet {numerals = "<1 6#s 3 5>"}
```

### Tritone substitution

This is just a 6 step offset of the current chord. This means the chord will be played 6 half steps lower then the original chord. To use it use the `#t` identifier.

```haskell
s1 = sheet {numerals = "<1 5#t 2 5>"}
```

Usually you would apply the tritone substitution of a dominant chord, basically the fifth of the diatonic chords of a major scale. But we are able to create secondary dominants at any time and then use the tritone substitution identifier on it:

```haskell
s1 = sheet {numerals = "<1 6 4 5#ts>"}
```

### Diminshed chords

This will play a full diminished chord one semitone below the original chord. When you apply "[1,3,5,7]" then will each note will be 3 half steps apart. This is a symmetrical chord and any note can be considered as the root.

You can use this to create chromatic harmony changes:

```haskell
s1 = sheet {numerals = "<1 4 5#d 5>"}
```

### Augmented chords

You can use augmented chords as a replacement of a dominant chord too. This will simply replace the chord with the same root.

```haskell
s1 = sheet {numerals = "<1 1#a 2 5#a>"}
```

### Chord inversions

The `inv` function for creating inversions is inspired by the official TidalCycles inversion mechanism (see [chord inversions](https://tidalcycles.org/docs/reference/harmony_melody/#chord-inversions)). There are two features that comes with the `inv`function that you can not achieve otherwise: 

1. You can create patterns for inversion independently from the underlying chords
2. You can use negative values two create lower versions of the inversions to create a chord movement without changing the underlying chords.

We start with the neutral value. This means you can use `0` to not change the chord at all:

```haskell
inverse "0" $ prog sheet "[1,3,5,7]"
```

Every value above 0 will add 12 to the lowest note value n times.

``` haskell
inverse "1" $ prog sheet "[1,3,5,7]"
inverse "1" $ note "[0,1,2,3]" -- note [1,2,3,12]
```

Every value below 0 will subtract 12 to the highest note value n times.

```haskell
inverse "-1" $ prog sheet "[1,3,5,7]"
inverse "-1" $ note "[0,1,2,3]" -- note [-9,0,1,2]
```

You can do it as often as you want. There is no limit by the number of notes:

```haskell
inverse "5" $ prog sheet "[1,3,5,7]"
inverse "5" $ note "[0,1,2,3]" -- note [13,14,15,24]
```

When you do not have a chord then it's like doing an octave offset of the note:

```haskell
inverse "[-1,1]" $ note "[0]" -- note [-12,12]
```

And of course you can use the mini notation to go wild:

```haskell
inverse "<-2 -1 0 1 2>" $ prog sheet "[1,3,5,7]"
```

### Drop voice chords

The `drop` function implements the drop voice chords. It lowers at least one specific note by an octave related to it's position in the chord. The neutral element is 0, but every value that is not expected will be ignored as well. 

```haskell
drop "0" $ prog sheet "[1,3,5,8]" -- equals  prog sheet "[1,3,5,8]"
```

A drop 2 chord means that the second highest note will be lowered by an octave, which means by 12 semitones and can be achieved in this way:

```haskell
drop "2" $ prog sheet "[1,3,5,8]"
drop "2" $ note "[0,1,2,3]" -- note [-10,0,1,3]
```

A drop 3 chord means that the third highest note will be lowered by an octave, which means by 12 semitones and can be achieved in this way:

```haskell
drop "3" $ prog sheet "[1,3,5,8]"
drop "3" $ note "[0,1,2,3]" -- note [-11,0,2,3]
```

A drop 4 chord means that the fourth highest note will be lowered by an octave, which means by 12 semitones and can be achieved in this way:

```haskell
drop "4" $ prog sheet "[1,3,5,8]"
drop "4" $ note "[0,1,2,3]" -- note [-12,1,2,3]
```

A drop 2+3 chord means that the second and third highest note will be lowered by an octave, which means by 12 semitones and can be achieved in this way:

```haskell
drop "2p3" $ prog sheet "[1,3,5,8]"
drop "2p3" $ note "[0,1,2,3]" -- note [-11,-10,0,3]
```

A drop 2+4 chord means that the second and fourth highest note will be lowered by an octave, which means by 12 semitones and can be achieved in this way:

```haskell
drop "2p4" $ prog sheet "[1,3,5,8]"
drop "2p4" $ note "[0,1,2,3]" -- note [-12,-10,1,3]
```

And of course you can use the mini notation to go wild (again):

```haskell
drop "<2 3 4 2p4 2p3>" $ prog sheet "[1,3,5,8]"
```

### Open Voice chords 

The open voice chord mechanism in TidalCycles is basically a drop 2+4 voicing and is equivalent to the use of `drop "2p4"`. But I added a shortcut to this that is applicable with a boolean pattern:

```haskell
open "t" $ prog sheet "[1,3,5,8]"
open "t" $ note "[0,1,2,3]" -- note [-12,-10,1,3]
open "<t f>" $ prog sheet "[1,3,5,8]"
```

### The limitations

- Syncopation is hard to achieve because you are bound to the chord changes. This is because rythm structures comes from both directions (|+|). This means from the numeral chord changes and the pattern you use to define which chord is played concretely  -> could be patternable if neccessary

## Custom conditional functions 

### Input and output mapping

The first function `linlin` maps a linear input to a linear output. This is more flexible then the [TidalCycles function range](https://tidalcycles.org/docs/reference/alteration/#range) because you can customize the input range as well. The structure of the function looks like this and will return a `Pattern Double`:

```haskell
noteMap (linlin inMin inMax outMin outMax)
```

 Every input value that is smaller then  `inMin` will be mapped to the `outMin`

```haskell
noteMap (linlin 12 36 0.6 1) (note "6") -- (0>1)|0.6
```

 Every input value that is bigger then  `inMax` will be mapped to the `outMax`

```haskell
noteMap (linlin 12 36 0.6 1) (note "40") -- (0>1)|1.0
```

The linear input will be mapped to the linear output. This means that the middle value between `inMin`and `inMax` will be exactly the middle value of `outMin` and `outMax`

```haskell
noteMap (linlin 12 36 0.6 1) (note "24") -- (0>1)|0.8
noteMap (linlin 12 36 0.6 1) (note "30") -- (0>1)|0.9
```

You can flip the `outMin` and `outMax` to flip the mapping. This means that a higher input value will create a smaller output value.

```haskell
noteMap (linlin 12 36 1 0.6) (note "30") -- (0>1)|0.7
noteMap (linlin 12 36 1 0.6) (note "6") -- (0>1)|1.0
noteMap (linlin 12 36 1 0.6) (note "40") -- (0>1)|0.6
```

Similiar to `linlin` there is the function `linexp` that has a similar functionality but maps a linear input to an exponential output. Unfortunately flip outMin and outMax will not make a difference.

```haskell
noteMap (linexp 12 36 0.6 1.0) (note "30") -- (0>1)|0.9658...
noteMap (linexp 12 36 0.6 1.0) (note "14") -- (0>1)|0.7365...
```

### Influence function values related to the pitch

You can use `linlin` and `linexp` to create a function that is related to the note of pattern. I.e. the higher the note value of a pattern is the louder it will become. This is interesting for creating arpeggios where the highest note will be highlighted.

```haskell
ngain pt = pt # (gain $ noteMap (linexp 12 36 0.6 1) pt)

ngain $ s "superpiano" # note "0 12 24 36"

-- (0>¼)-1  | gain: 0.6f, note: 0.0n (c5), s: "superpiano"
-- 0-(¼>½)-1| gain: 0.6f, note: 12.0n (c6), s: "superpiano"
-- 0-(½>¾)-1| gain: 0.9187385282334165f, note: 24.0n (c7), s: "superpiano"
-- 0-(¾>1)  | gain: 1.0f, note: 36.0n (c8), s: "superpiano"
```

But of course you can use this to influence any kind of function that accepts `Pattern Double` like `cutoff`, `start`, `end` or `speed`:

```haskell
ncutoff pt = pt # (cutoff $ noteMap (linlin 0 32 4000 500) pt)

ncutoff $ s "superpiano" # note "0 12 24 36"

-- (0>¼)-1  | cutoff: 4000.0f, note: 0.0n (c5), s: "superpiano"
-- 0-(¼>½)-1| cutoff: 2687.5f, note: 12.0n (c6), s: "superpiano"
-- 0-(½>¾)-1| cutoff: 1375.0f, note: 24.0n (c7), s: "superpiano"
-- 0-(¾>1)  | cutoff: 500.0f, note: 36.0n (c8), s: "superpiano"
```

## Live coding with parts and segments

I started to create a custom user interface to address some of my personal needs. Here are some of my requirements:
- Be able to create segments for different musicals elements (like lead, pad or drums)
- Be able to create parts and switch between them (with code and/or with midi signals to play an instrument in parallel) for longer compositions -> coming back to live coded elements.
- Segments of parts should stay close together because they should be live coded too.
- Segments of parts should be silenced when they are not defined.
- Use dj techniques like permanent filters (custom), transitions (standard TC), tempo changes and key (different topic - see below)
- Using odd time signatures for different parts

For every point I will give you some examples. Then it should become more clear. I made a lot of design decisions which came with some advantages and disadvanteges. This is my individual approach but maybe there is something useful for you too.

### The template

I want to introduce the words segments and parts for the template.

- segement: is an element of the stacker function. It's a so called tuple,with the structure "(name, pattern)"
- part: a stacker with it's content. You can switch between multiple stacker and play everything inside it.

```haskell
do
let
    part = "1" -- <- this can be changed with midi/osc
    parts = transformStacker [
        stacker [
          ("lead", s "superpiano" <| n "c a f e")
          , ("drums", s "[bd, hh]")
        ]
      ]
    --
    seg orb key = orbit orb <| ur 8 (part)  (parts ! key) []
    --
    --
d1 $ stack [seg 0 "lead" , seg 1 "bass", seg 2 "key", seg 3 "pad", seg 4 "arp", seg 5 "drums" ]
```

The keys for the segments, that can be used insider `stacker`, are defined inside the `transformStacker` function under `allowedAndNeededKeys` in `./code.tidal`.

### Defining parts

Just add a new `stacker` to the `transformStacker` and change/add segements to your needs:

```haskell
parts = transformStacker [
    stacker [
      ("lead", s "superpiano" <| n "c a f e")
      , ("drums", s "[bd*2, hh*4]")
    ]
    , stacker [
      ("lead", s "superpiano" <| n "f f e*2 c")
      , ("drums", s "[bd(3,8), hh*8]")
    ]
  ]
```
### Switching between parts
When the `transformStacker` function contains more then one `stacker` function with a list,
you can switch between them in this line:

```haskell
seg orb key = orbit orb <| ur 8 ("1")  (parts ! key) []
```

I simply use the `ur` function mechanism to switch between the parts. When you change
the `1` to a `2`, then you use the elements of the second stacker.

### Duration of a loop
The `ur` function contains a value for the duration of the pattern. In this example
the duration is `8` cycles:
```haskell
seg orb key = orbit orb <| ur 8 ("1")  (parts ! key) []
```
This means, that a longer loop would be abort and played from the beginning after 8 cycles.

But this also means, that you can use this creatively too. Let's look into the following pattern:
```haskell
parts = transformStacker [
    stacker [
      ("lead", slow 5 $ s "superpiano" <| n "c a f f e")
    ]
  ]
```

The notes (each per cycles) would be played as `c a f f e c a f` and then repeated over and over again.

### Auto silence
When an element in the list is not defined or commented out, then the part is automatically filled with the function `silence`:
```haskell
parts = transformStacker [
    stacker [
      ("drums", s "[bd, hh]")
      -- ,("lead", s "superpiano" <| n "c a f e")
    ]
  ]
```
### Orbit routing
I just use `d1` and the function `orbit` for the audio routing. That's why you can not use
solo and mute mechanism. It would be not a problem to slighlty adjust the code, to fix this.
But this is a design decision for applying different effects on different levels (see next sections).

That's why I introduced this part of the template:
```haskell
d1 $ stack [seg 0 "lead" , seg 1 "bass", seg 2 "key", seg 3 "pad", seg 4 "arp", seg 5 "drums" ]
```

### Effects (local, cross-parts, global)

Because I introduced segments and parts it can be interesting to apply effects and patterns on different levels of the arrangement.

A local effects is something you use in one segment in a part:

```haskell
-- Local effect 
-- Just for on segment in a part
parts = transformStacker [
    stacker [
      ("lead", s "superpiano" <| n "c a f e" # cutoff 4000)
      , ("drums", s "[bd, hh]")
    ]
  ]
```

When you want to apply effects on one specific segments but share it across parts, you can do it directly in the `stack` function of `d1`

```haskell
-- Cross-part effect
-- Is applied on a specific segment no matter what part is active, 
d1 $ stack [seg 0 "lead" # cutoff 4000, seg 5 "drums" ]
```

And you are not limited to add simple effects. You could write patterns that stay across the parts. I used this for example, to create arpeggios that exists longer then a part.

``` haskell
-- Fun fact
-- You can use this approach to specify patterns that will stay for a segment independently of the active part:
d1 $ stack [orbit 0 <| s "superpiano" <| n "c a f e", seg 5 "drums" ]
```

And you can use an effect globally to create interesting transitions or breaks, i.e. with filter:

```haskell
-- Global effect
-- Is applied on a every segment no matter what part is active, 
d1 $ stack [seg 0 "lead", seg 5 "drums" ] # cutoff 4000
```

### Permanent effect transitions

I introduced the two functions `lpf'` and `hpf'`  for a some kind of a "permanent effect transition".
This uses the `envL` function inside to do a linear interpolation between 0 and 1 during the first four cycles and then stays a the destintion value. This continuous value is multiplied with a start value (lpf' = 15000 and hpf' = 0) and then it will interpolate until the passed value is reached. The interpolation starts directly with the last evaluation of the pattern.

```haskell
do
t <- toRational <$> getnow
d1 $ s "superpiano" <| n "c a f e" # hpf' 700 t
```

As you can see, you need to introduce the `t` variable. To avoid this would unfortunately change the interface of the `d1` function. This is the reason why I keep it as it is.

### Parts cps and odd time signatures

When you want to work with odd time signatures you could do it at any time with everything that TidalCycles provide you. Because a cycle do not have a time signature grid and doesn't know what this means. The feeling of odd time signatures comes simply from the pattern. When you want 4 beats then you use 4 or 8 elements. When you want to use 3 beats then you use 3 or 6 elements and so on.

So what's the matter here? When I work with parts and segments I want to change the feeling of the meter but try to stay at the same feeled tempo. When I change from a 4/4 time signature to a 7/8 time signature, then I add these values in my `mapfx` function. This uses the `fx` functionality from the `ur` function. This means you need to write `1:1` instead of `1` to switch parts and tempo. With the last digit, you are able to change the tempo, based on the value of it's position in the `mapfx` list. The value is multiplied (and divided by 1) with the first value, which is simply the cps. In this way you can treat the second parameter of `mapfx` as your time signature which means that `4/4` is you `4/4` time signature and the value `7/8` is actually a `7/8` time signature. 

```haskell
do
let
    fx = mapfx 0.50 [4/4, 7/8] -- <- this one was added
    parts = transformStacker [
        stacker [
          ("lead", s "superpiano" <| n "c a f e")
          , ("drums", s "[bd, hh]")
        ]
        ,stacker [
          ("drums", s "[bd(2,7), hh(4,7)]") -- <- I like this use of the eucledian algorithm for odd time signatures
        ]
      ]
    --
    seg orb key = orbit orb <| ur 8 ("1:1")  (parts ! key) fx -- <- this one was added
    --
    --
d1 $ stack [seg 0 "lead" , seg 1 "bass", seg 2 "key", seg 3 "pad", seg 4 "arp", seg 5 "drums" ]
```

## Tips and hints for live coding with longer sets
For live coding sets that are longer, i.e. 30-45 minutes or more, you certainly want to make music with a variety of different sounds for drums, leads, pads and more. The more elements you have, the more names to access (samples, synthesizers, MIDI) you have to remember. This is what I call "mental mapping" and I try to reduce it without limiting myself in the variety of possibilities.

### Drumbank mapping

Let's take the example with the drums. Here you could use different sample banks with different names. The more sample banks you want to use, the more complex it becomes to remember the names of the samples. But for me `bd` should always be the basedrum and `hh` should play the sound of a high hat.

The code under `./code.scd` is needed for the custom mapping and this TC code:

```haskell
-- Drum bank
drumBank = pI "drumBank"
```

Then you can start to create a sample bank like `summer` and add subfolders like `summerbd`, `summerhh` and `summersn`.

After this you could start to use it like

```haskell
d1 $ s "[bd*2, hh*4, ~ sn]" # drumBank 1
```

Usually I use this with my setup for larger sets and append the `drumBank` as a cross-part effect or global effect: 

```haskell
d1 $ stack [seg 0 "lead" , seg 1 "bass", seg 2 "key", seg 3 "pad", seg 4 "arp", seg 5 "drums" # drumBank 1 ]
```

Finally I can use it in my `drums` segment and don't need to think about the drum mapping anymore:

```haskell
("drums", s "[bd*2, hh*4, ~ sn]")
```

