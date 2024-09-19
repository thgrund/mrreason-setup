# Live coding with parts and segments

I started to create a custom user interface to address some of my personal needs. Here are some of my requirements:
- Be able to create segments for different musicals elements (like lead, pad or drums)
- Be able to create parts and switch between them (with code and/or with midi signals to play an instrument in parallel) for longer compositions -> coming back to live coded elements.
- Segments of parts should stay close together because they should be live coded too.
- Segments of parts should be silenced when they are not defined.
- Use dj techniques like permanent filters (custom), transitions (standard TC), tempo changes and key (different topic - see below)
- Using odd time signatures for different parts

For every point I will give you some examples. Then it should become more clear. I made a lot of design decisions which came with some advantages and disadvanteges. This is my individual approach but maybe there is something useful for you too.

## The template

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

## Defining parts

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
## Switching between parts
When the `transformStacker` function contains more then one `stacker` function with a list,
you can switch between them in this line:

```haskell
seg orb key = orbit orb <| ur 8 ("1")  (parts ! key) []
```

I simply use the `ur` function mechanism to switch between the parts. When you change
the `1` to a `2`, then you use the elements of the second stacker.

## Duration of a loop
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

## Auto silence
When an element in the list is not defined or commented out, then the part is automatically filled with the function `silence`:
```haskell
parts = transformStacker [
    stacker [
      ("drums", s "[bd, hh]")
      -- ,("lead", s "superpiano" <| n "c a f e")
    ]
  ]
```
## Orbit routing
I just use `d1` and the function `orbit` for the audio routing. That's why you can not use
solo and mute mechanism. It would be not a problem to slighlty adjust the code, to fix this.
But this is a design decision for applying different effects on different levels (see next sections).

That's why I introduced this part of the template:
```haskell
d1 $ stack [seg 0 "lead" , seg 1 "bass", seg 2 "key", seg 3 "pad", seg 4 "arp", seg 5 "drums" ]
```

## Effects (local, cross-parts, global)

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

## Permanent effect transitions

I introduced the two functions `lpf'` and `hpf'`  for a some kind of a "permanent effect transition".
This uses the `envL` function inside to do a linear interpolation between 0 and 1 during the first four cycles and then stays a the destintion value. This continuous value is multiplied with a start value (lpf' = 15000 and hpf' = 0) and then it will interpolate until the passed value is reached. The interpolation starts directly with the last evaluation of the pattern.

```haskell
do
t <- toRational <$> getnow
d1 $ s "superpiano" <| n "c a f e" # hpf' 700 t
```

As you can see, you need to introduce the `t` variable. To avoid this would unfortunately change the interface of the `d1` function. This is the reason why I keep it as it is.

## Parts cps and odd time signatures

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
