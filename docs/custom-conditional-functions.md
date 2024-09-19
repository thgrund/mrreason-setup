# Custom conditional functions

## Input and output mapping

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

## Influence function values related to the pitch

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
