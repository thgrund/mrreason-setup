# MrReason Setup
Design, ideas and motivation for my TidalCycles setup.

I have divided this post into different sections.
Here it is about quite different aspects like among others the live coding of a larger set with multiple parts and transitions (live coding paired with code DJing) or the application of western music theory.

To be able to use the described concepts, the code under `./code.tidal` should be executed first.

Table of content:

- [Applying modern western music theory](#applying-western-music-theory)
- [Live coding with parts and segments (for longer compositions)](#live-coding-with-parts-and-segments)
- [Tips and hints for live coding with longer sets](#tips-and-hints-for-live-coding-with-longer-sets)

## Applying western music theory

My motivation was to make some rules accessible via TidalCycles using the system of western music theory that I am familiar with. However, I attach great importance to defining and transforming chord progressions and notes with patterns, in order to be able to use the strengths of TidalCycles flexibly here.

### The 'Sheet' datatype

I created a template based on the `Sheet` definition that you can find under  `./code.tidal`:

```haskell
sheet = Sheet {
  key = "c",
  mode = "major",
  numerals = "1",
  sndDom = "f",
  dim = "f",
  aug = "f",
  triSub = "f"
}
```

When the record `sheet` is available, you can just create a new one and change just the elements you need to change:

```haskell
s1 = sheet {numerals = "<1 1 4 5>"}
```

I will explain the usage of each field below. Every field is made of patterns. This means you can use the mini-notation on every field. But you could use functions here too:

``` haskell
s1 = sheet {numerals = every 2 (rev) "1 5"}
```

### Chords and melodies

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

When I need the harmony to stay on a similar level, then I create inversions. But I just limit the range of notes with the modulo operator like this:

```haskell
d1 $ s "superpiano" <| progWith (|% 12) s1 "[1,3,5,7]"
```

It's not explicitly used for voice leading but to avoid an up or down movement in the harmony (if you like so).
Here is an example for the fourth degree of the c major scale:

```haskell
s1 = sheet {numerals = "4"}

s "superpiano" <| progWith (|% 12) s1 "[1,3,5,7]"

-- (0>1)|note: Note {unNote = 5.0}n, s: "superpiano"
-- (0>1)|note: Note {unNote = 9.0}n, s: "superpiano"
-- (0>1)|note: Note {unNote = 0.0}n, s: "superpiano"
-- (0>1)|note: Note {unNote = 4.0}n, s: "superpiano"

-- This similar to the second inversion of Fmaj7: note "f'maj7'ii"
-- But the order is not correct. This is just a problem when you try 
-- to arpeggiate the result
```

And I really like that you can easily write melodies or sequences that are related to the key and the chord changes (playing with the changes) but the melody notes don't need to be the exact notes from the chords:

```haskell
d2 $ s "superpiano" <| prog s1 "1 5 7@2 3 6 8@2" |+ note 12
```

### Same key - different mode

The melody doesn't need to use them same mode, key or numerals. One use case is to play a chord progression in major and use the minor pentatonic for the melody like this:

```haskell
do
let s1 = sheet {mode = "major", numerals = "<1 1 4 5>"}
    s2 = s1 {mode = "minPent" }
d1 $ s "superpiano" <| prog s1 "[1,3,5]"
d2 $ s "superpiano" <| prog s2 "1 2 3@2 2 3 4@2"
```

In this way you play you could say "on top of the changes" instead of "with the changes":

```haskell
s2 = s1 {mode = "minPent", numerals = "1" }
```

### Changing the key

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

### Borrowing Chords

When you want to borrow a chord explicitly from an other key, then you simply change the mode

```haskell
s1 = sheet {mode = "<phrygian!3 minor>", numerals = "<1 2 3 5>"}
```

### Secondary dominants

The secondary dominant will be played 7 half steps higher then the original chord would be. This is simply a major chord with a (possible) dominant seven (V7) 7 half steps above the original chord.

```haskell
s1 = sheet {numerals = "<1 6 3 5>", sndDom = "<f t f f>"}
```

### Tritone substitution

This is just an 6 step offset of the current chord and can be used at any time.

```haskell
s1 = sheet {numerals = "<1 1 4 5>", triSub = "<f!3 t>"}
```

But usually you would apply the tritone substitution of a dominant chord, basically the fifth of the diatonic chords of a major scale. But we are able to create secondary dominants at any time and then use the tritone substitution concept on it:

```haskell
s1 = sheet {numerals = "<1 6 4 5>", triSub = "<f!3 t>", sndDom = "<f t f f>"}
```

### Diminshed chords

This will play a full diminished chord one semitone below the original chord. When you apply "[1,3,5,7]" then will each note will be 3 half steps apart. This is a symmetrical chord and any note code be considered as the root.

You can use this to create chromatic harmony changes:

```haskell
s1 = sheet {numerals = "<1 4 5 5>", dim = "<f f t f>"}
```

### Augmented chords

You can use augmented chords as a replacement of a dominant chord too. This will simply replace the chord with the same root.

```haskell
s1 = sheet {numerals = "<1 1 2 5>", aug = "<f t f t>"}
```

### The limitations

- Syncopation is hard to achieve because you are bound to the chord changes. This is because rythm structures comes from both directions (|+|). This means from the numeral chord changes and the pattern you use to define which chord is played concretely  -> could be patternable if neccessary

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

When you want to work with odd time signatures you could do it at any time. Because a cycle do not have some kind of a time signature grid and doesn't know what this means. The feeling of odd time signatures comes simply from the pattern. When you want 4 beats then you use 4 or 8 elements. When you want to use 3 beats then you use 3 or 6 elements and so on.

So what's the matter here? When I work with parts and segments I want to change the feeling of the meter but try to stay at the same feeled tempo. When I change from a 4/4 time signature to a 7/8 time signature, then I add these values in my `mapfx` function. This uses the `fx` functionality from the `ur` function. This means you need to write `1:1` instead of `1` to switch parts and tempo. With the last digit, you are able to change the tempo, based on the value of it's position in the `mapfx` list. The value is multiplied with the first value, which is simply the cps.

```haskell
do
let
    fx = mapfx 0.50 [1, 8/7] -- <- this one was added
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

