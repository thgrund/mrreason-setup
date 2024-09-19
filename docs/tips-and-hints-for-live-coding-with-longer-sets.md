# Tips and hints for live coding with longer sets
For live coding sets that are longer, i.e. 30-45 minutes or more, you certainly want to make music with a variety of different sounds for drums, leads, pads and more. The more elements you have, the more names to access (samples, synthesizers, MIDI) you have to remember. This is what I call "mental mapping" and I try to reduce it without limiting myself in the variety of possibilities.

## Drumbank mapping

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
