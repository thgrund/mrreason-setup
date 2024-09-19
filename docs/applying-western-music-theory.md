# Applying western music theory

My motivation was to make some rules accessible via TidalCycles using the system of western music theory that I am familiar with. However, I attach great importance to defining and transforming chord progressions and notes with patterns, in order to be able to use the strengths of TidalCycles flexibly here.

## How it works

### The 'Sheet' datatype

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

### Apply functions to the complete progression

It's possible to apply functions to the complete progression. Unfortunately you can not directly apply them in `numerals`. For this I added a `functions` property to the `Sheet` datatype. This i.e. works:

```haskell
s1 = sheet {numerals = "<1 1 4 5>", functions = fast 4}
```

And you can even compose multiple functions like this:

```haskell
s1 = sheet {numerals = "<1 1 4 5>", functions = rev . (fast 4)}
```

If you don't specify the `functions` property, then the identity function `id` will be used. This means no change to the original chord progression will be applied.

## Numerals Identifier

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
| H1-H7      | Uses the modes of h                                          |

## Borrowing Chords

When you want to borrow a chord explicitly from an other key, then you simply change the mode

```haskell
s1 = sheet {mode = "<phrygian!3 minor>", numerals = "<1 2 3 5>"}
```

The same is achievable with an identifier. I.e. you can use the phrygian mode as a default mode and replace the last value in numerals with the aeolian mode (means minor):

```haskell
s1 = sheet {mode = "phrygian", numerals = "<1 2 3 5#A>"}
```

## Secondary dominants

The secondary dominant will be played 5 half steps lower then the original chord would be. This is simply a major chord with a (possible) dominant seven chord (V7) 7 half steps above the original chord. To use it use the `#s` identifier in numerals on the value you want to apply this.

```haskell
s1 = sheet {numerals = "<1 6#s 3 5>"}
```

## Tritone substitution

This is just a 6 step offset of the current chord. This means the chord will be played 6 half steps lower then the original chord. To use it use the `#t` identifier.

```haskell
s1 = sheet {numerals = "<1 5#t 2 5>"}
```

Usually you would apply the tritone substitution of a dominant chord, basically the fifth of the diatonic chords of a major scale. But we are able to create secondary dominants at any time and then use the tritone substitution identifier on it:

```haskell
s1 = sheet {numerals = "<1 6 4 5#ts>"}
```

## Diminshed chords

This will play a full diminished chord one semitone below the original chord. When you apply "[1,3,5,7]" then will each note will be 3 half steps apart. This is a symmetrical chord and any note can be considered as the root.

You can use this to create chromatic harmony changes:

```haskell
s1 = sheet {numerals = "<1 4 5#d 5>"}
```

## Augmented chords

You can use augmented chords as a replacement of a dominant chord too. This will simply replace the chord with the same root.

```haskell
s1 = sheet {numerals = "<1 1#a 2 5#a>"}
```

## Chord inversions

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

## Drop voice chords

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

## Open Voice chords

The open voice chord mechanism in TidalCycles is basically a drop 2+4 voicing and is equivalent to the use of `drop "2p4"`. But I added a shortcut to this that is applicable with a boolean pattern:

```haskell
open "t" $ prog sheet "[1,3,5,8]"
open "t" $ note "[0,1,2,3]" -- note [-12,-10,1,3]
open "<t f>" $ prog sheet "[1,3,5,8]"
```

## The limitations

- Syncopation is hard to achieve because you are bound to the chord changes. This is because rythm structures comes from both directions (|+|). This means from the numeral chord changes and the pattern you use to define which chord is played concretely  -> could be patternable if neccessary
