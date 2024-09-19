# MrReason Setup
Design, ideas and motivation for my TidalCycles setup.

I have divided this post into different sections.
Here it is about quite different aspects like among others the live coding of a larger set with multiple parts and transitions (live coding paired with code DJing) or the application of western music theory.

To be able to use the described concepts, you need to be in the directory with this repositories content and install this library with `cabal v2-install --lib` . Then you can import the libraries i.e. in your `BootTidal.hs`
```haskell
import Sound.MrReason.Setup
import Sound.Tidal.MIDI
```

Beside the concepts I describe in this repository, I highly frequently use some other projects I created in combination with them:

- [SuperDirtMixer](https://github.com/thgrund/SuperDirtMixer/tree/mrreason-setup)
- [TidalLooper](https://github.com/thgrund/tidal-looper)
- [TidalVST](https://github.com/thgrund/TidalVST/tree/mrreason-setup)
- [Tidal Pedalboard effects](https://github.com/thgrund/tidal-pedalboard-effects)

Table of content:

- [Infrastructure overview](./docs/infrastructure-overview.md)
- [Applying modern western music theory](./docs/applying-western-music-theory.md)
- [Custom conditional functions](./docs/custom-conditional-functions.md)
- [Turn Tidal patterns to MIDI score](./docs/tidal-midi.md)
- [Live coding with parts and segments (for longer compositions)](./docs/live-coding-with-parts-and-segments.md)
- [Tips and hints for live coding with longer sets](./docs/tips-and-hints-for-live-coding-with-longer-sets.md)
