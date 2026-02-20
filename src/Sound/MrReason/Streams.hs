module Sound.MrReason.Streams (wrappedTidalStream, wrappedSuperDirtMixerStream, dx, hush) where

import Data.Map ((!))
import Sound.MrReason.Setup (transformStacker, ur')
import Sound.Tidal.Context

-- vstTarget = Target {oName = "hydra", oAddress = "127.0.0.1", oHandshake = True, oPort = 57101, oBusPort = Just 3338, oLatency = 0.2, oSchedule = Pre BundleStamp, oWindow = Nothing}
superDirtMixerTarget = Target {oName = "superDirtMixer", oAddress = "localhost", oPort = 57121, oLatency = 0.2, oSchedule = Live, oWindow = Nothing, oHandshake = False, oBusPort = Nothing}

oscplay = OSC "/SuperDirtMixer" Named {requiredArgs = ["orbit"]}

oscmap = [(superDirtMixerTarget, [oscplay])]

wrappedTidalStream = startStream (defaultConfig {cFrameTimespan = 1 / 20}) [(superdirtTarget {oLatency = 0.2, oHandshake = True}, [superdirtShape])]

wrappedSuperDirtMixerStream = startStream (defaultConfig) oscmap

dx globalFx part dur fx ping stackers = do
  tidal <- wrappedTidalStream
  superDirtMixer <- wrappedSuperDirtMixerStream
  let p = streamReplace tidal
      d1 = p 1 . (|< orbit 0)
      d2 = p 2 . (|< orbit 1)
      d3 = p 3 . (|< orbit 2)
      d4 = p 4 . (|< orbit 3)
      d5 = p 5 . (|< orbit 4)
      d6 = p 6 . (|< orbit 5)
      d7 = p 7 . (|< orbit 6)
      d8 = p 8 . (|< orbit 7)
      d9 = p 9 . (|< orbit 8)
      d10 = p 10 . (|< orbit 9)
      d11 = p 11 . (|< orbit 10)
      d12 = p 12 . (|< orbit 11)
      d13 = p 13 . (|< orbit 12)
      d14 = p 14 . (|< orbit 13)
      x1 = streamReplace superDirtMixer 1 . (|< orbit 0)
      x2 = streamReplace superDirtMixer 2 . (|< orbit 1)
      x3 = streamReplace superDirtMixer 3 . (|< orbit 2)
      x4 = streamReplace superDirtMixer 4 . (|< orbit 3)
      x5 = streamReplace superDirtMixer 5 . (|< orbit 4)
      x6 = streamReplace superDirtMixer 6 . (|< orbit 5)
      x7 = streamReplace superDirtMixer 7 . (|< orbit 6)
      x8 = streamReplace superDirtMixer 8 . (|< orbit 7)
      x9 = streamReplace superDirtMixer 9 . (|< orbit 8)
      x10 = streamReplace superDirtMixer 10 . (|< orbit 9)
      x11 = streamReplace superDirtMixer 11 . (|< orbit 10)
      x12 = streamReplace superDirtMixer 12 . (|< orbit 11)
      x13 = streamReplace superDirtMixer 13 . (|< orbit 12)
      x14 = streamReplace superDirtMixer 14 . (|< orbit 13)
      parts = transformStacker stackers
      seg key = ur' dur (part) (parts ! key) fx
  d1 $ globalFx $ seg "lead"
  x1 $ globalFx $ seg "leadFx"
  d2 $ globalFx $ seg "bass"
  x2 $ globalFx $ seg "bassFx"
  d3 $ globalFx $ seg "key"
  x3 $ globalFx $ seg "keyFx"
  d4 $ globalFx $ seg "pad"
  x4 $ globalFx $ seg "padFx"
  d5 $ globalFx $ seg "arp"
  x5 $ globalFx $ seg "arpFx"
  d6 $ -0.04 <~ s ping
  d7 $ globalFx $ seg "rGit"
  x7 $ globalFx $ seg "rGitFx"
  d8 $ globalFx $ seg "lGit" # overgain ((rand * 0.025) - 0.05)
  x8 $ globalFx $ seg "lGitFx"
  d9 $ globalFx $ seg "loop" # overgain ((rand * 0.025) - 0.05)
  x9 $ globalFx $ seg "loopFx"
  d10 $ globalFx $ seg "fx"
  x10 $ globalFx $ seg "fxFx"
  d11 $ globalFx $ unfix (# silence) (s (pure "bd")) $ seg "drums" # overgain ((rand * 0.1) - 0.05)
  x11 $ globalFx $ seg "drumsFx"
  d12 $ globalFx $ unfix (# silence) (s (pure "[hh, cr, rd, hho, ch, mi, kontaktcr]")) $ seg "drums" # overgain ((rand * 0.1) - 0.05)
  x12 $ globalFx $ seg "drumsFx"
  d13 $ globalFx $ unfix (# silence) (s (pure "[sn, clap, snip, wood]")) $ seg "drums" # overgain ((rand * 0.1) - 0.05)
  x13 $ globalFx $ seg "drumsFx"
  d14 $ globalFx $ unfix (# silence) (s (pure "[tom, djembe]")) $ seg "drums" # overgain ((rand * 0.1) - 0.05)
  x14 $ globalFx $ seg "drumsFx"

hush = do
  tidal <- wrappedTidalStream
  superDirtMixer <- wrappedSuperDirtMixerStream
  streamHush tidal
  streamHush superDirtMixer
