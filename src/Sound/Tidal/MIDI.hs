--module Sound.Tidal.MIDI (TidalMIDIScore (..), midiScore, songName, urPattern, globalBPM, scoreDuration) where
module Sound.Tidal.MIDI (TidalMIDIScore (..), midiDrumTrack, midiNoteTrack, midiFile, transformDur) where

import Data.List (groupBy, nub, partition)
import Data.Map (lookup, insert)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)

import Sound.Tidal.Context
import Sound.Tidal.Utils (enumerate)

import Data.Ratio ((%))
import Data.List (foldl', sortOn, sortBy)
import qualified Data.Map as Map
import Sound.Tidal.Context (Arc(..), Event(..), Pattern, Value(..))

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.File            as MidiFile
import qualified Sound.MIDI.File.Event      as Event
import qualified Sound.MIDI.File.Event.Meta as MetaEvent

import qualified Data.EventList.Relative.TimeBody as EventList

import Sound.MIDI.General

import Sound.MrReason.Setup

data TidalMIDIScore = TidalMIDIScore {
    songName :: String,
    tsTimeSignatures :: [(Time, Time, Int)],
    globalBPM :: Double,
    urPattern :: Pattern String,
    scoreDuration :: Time
  }

midiScore = TidalMIDIScore {
  songName = "",
  tsTimeSignatures = [(1 % 1, 4, 2)],
  globalBPM = 120.0,
  urPattern = silence,
  scoreDuration = 4
}

groupedEvents pt arc = groupBy (\ev1 ev2 -> whole ev1 == whole ev2) (sortOn part $ queryArc pt arc)

extractNote (VN x) = unNote x
extractNote _ = 0

extractString (VS x) = x
extractString _ = ""

transformToMidi (Event c (Just (Arc s e)) _ v) =
  let noteValue = fromMaybe (VN 0) (Data.Map.lookup "note" v)
  in (s, e , extractNote noteValue)
transformToMidi _ = error "Unexpected event without arc"

-- Helper function to extract the note value
convertToDesiredOutput = map (map transformToMidi)

-- Tick = 1 quarter note
tick = 480
-- 4 Tick represent one bar in 4/4
ticksPerCycle = tick * 4
chan = ChannelMsg.toChannel 3
vel  = VoiceMsg.toVelocity 64

durationToTicks :: Integral b => Rational -> b
durationToTicks dur = round (fromRational dur * fromIntegral ticksPerCycle)

addDurSum :: Num t => t -> [(a, t, c)] -> [(a, t, c, t)]
addDurSum acc [] = []
addDurSum acc ((a, b, c):[]) = (a,b,c, acc + b) : (addDurSum (acc + b) [])
addDurSum acc ((a,b,c):xs) = (a,b,c, acc + b) : (addDurSum (acc + b) xs)

-- TODOS
-- Just a apply the offset on the first element
calculateOffsets events = reverse $ addOffsets $ reverse events
  where addOffsets [] = []
        addOffsets (x:[]) = [((\(a, b, c) -> (a, b, c, a)) (head x)) : (map (\(a, b, c) -> (a, b, c, 0 % 1)) (tail x))] ++ addOffsets []
        addOffsets (x:y:xs) = [
            ((\(a, b, c) -> (a, b, c, a - (snd3 (head y)))) (head x)) :
            (map (\(a, b, c) -> (a, b, c, 0)) (tail x))
          ] ++ addOffsets (y : xs)
        snd3 (_, b, _) = b

sortNoteOffEventsByEndTime :: Ord a1 => (a2, a1, c1) -> (a3, a1, c2) -> Ordering
sortNoteOffEventsByEndTime (_, b1, _) (_, b2, _)
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = EQ

  {- | The `transformDur` transforms the time of events in TidalCycles in a way, that it would fit
  a specific time signature in midi. The time signature will be applied till the first value of the
  ts head is reached.

  Example:

  @
  pt = note "<c ~! c*2>"
  arc = Arc 0 4
  @

  This will be transformed to:

  @
  events = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
           ,("noteOn",2 % 1,0.0,3 % 1),("noteOff",1 % 2,0.0,7 % 2)
           ,("noteOn",0 % 1,0.0,7 % 2),("noteOff",1 % 2,0.0,4 % 1)]
  @

  With this time signature list:
  @
  ts = [(1 % 1, 1, 2),(2 % 1, 7, 2), (3 % 1, 9 , 3), (4 % 1, 2 , 2)]
  @

  You can then call transformDur to transform the TidalCycles events to match the defined time signature.
  Otherwise every pattern will be transformed to 4/4 time signature.

  @
  transformDur ts events
  -- result:
  --  [("noteOn",0 % 1,0.0),("noteOff",1 % 4,0.0)
  --  ,("noteOn",23 % 8,0.0),("noteOff",1 % 4,0.0)
  --  ,("noteOn",0 % 1,0.0),("noteOff",1 % 4,0.0)]
  @
  -}

transformDur [] [] = []
transformDur [] ((a2,b2,c2,_):[]) =  (a2,b2,c2) : (transformDur [] [])
transformDur [] ((a2,b2,c2,_):xs) = ((a2,b2,c2)) : (transformDur [] xs)
transformDur ((a1, b1, c1):xs) [] = []  -- Second list empty, stop transformation
transformDur ((a1, b1, c1):xs) (("noteOff", dur, pitch, cycleCount):ys)
  | a1 >= cycleCount = ("noteOff", calcRest dur  b1 c1, pitch) : transformDur ((a1, b1, c1):xs) ys
  | otherwise          = transformDur xs (("noteOff", dur, pitch, cycleCount):ys)
    where
      calcRest dur nom den = (nom / (2 ^ den)) * dur
transformDur ((a1, b1, c1):xs) (("noteOn", dur, pitch, cycleCount):ys)
  | a1 >= cycleCount = ("noteOn", calcRest dur  b1 c1, pitch) : transformDur ((a1, b1, c1):xs) ys
  | (fst splitted) == [] =  transformDur (snd calculatedRestDuration) (("noteOn", dur, pitch, cycleCount) : ys)
  | a1 < cycleCount = (fst calculatedRestDuration) : transformDur (snd calculatedRestDuration) ys
  -- | otherwise = transformDur xs ((eventType, dur, pitch, cycleCount):ys)
  where
    calcRest dur nom den = (nom / (2 ^ den)) * dur
    calculatedRestDuration = calcRestDuration ("noteOn", dur, pitch, cycleCount) ((a1, b1, c1):xs)
    splitted = Data.List.partition (\(a,_,_) -> a <= cycleCount) (filter (\(a,_,_) -> cycleCount - dur < a) ((a1, b1, c1):xs))
    calcRestDuration ("noteOn", x, y, z) xs = (("noteOn", calcDurationSum (calcEventDuration (reverse $ fst splitted))  + (calcExtraRest  (reverse $ fst splitted) (snd splitted) z) , y), snd splitted)
      where
            calcEventDuration [] = []
            calcEventDuration ((a1,b1,c1) : (a2, b2, c2) : xs) = (a1 - a2, b1, c1) : calcEventDuration ((a2, b2, c2) : xs)
            calcEventDuration ((a,b,c) : xs) = (a - (z - x), b, c) : calcEventDuration []
            calcDurationSum xs = foldl (\ acc (a, b, c) -> ((b / 2 ^ c) * a) + acc) 0 xs
            calcExtraRest [] [] _ = 0
            calcExtraRest _ [] _ = 0
            calcExtraRest ((x,_,_):xs) ((_, nom, den):ys) z = if (z - x >= 0) then calcRest (z - x) nom den else 0

tcPatternToMidiEventTransformer pt arc ts= map (convert) groupedMidiNoteEvents
  where
    noteEventList = map nub $ convertToDesiredOutput $ groupedEvents (pt) (arc)
    groupedMidiNoteEvents = transformDur ts (addDurSum 0 $ calculateOffsets $ sortBy (sortNoteOffEventsByEndTime) $ (map (\(s, e, v) -> ("noteOff", e, v)) (concat noteEventList)) ++ (map (\(s, e, v) -> ("noteOn",s, v)) (concat noteEventList)))
    calculateOffsets events = reverse $ addOffsets $ reverse events
    addOffsets [] = []
    addOffsets (x:[]) = x : addOffsets []
    addOffsets ((a1, b1, c1):(a2, b2, c2):xs) = (a1, b1 - b2, c1) : addOffsets ((a2, b2, c2) : xs)
    convert (eventType, duration, pitch) =
      let msgType = if eventType == "noteOn" then VoiceMsg.NoteOn else VoiceMsg.NoteOff
      in  (durationToTicks duration, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (msgType (VoiceMsg.toPitch ((round pitch) + 60)) vel))))

midiNoteTrack instrument pt midiScore=
   let chan = ChannelMsg.toChannel 3
       vel  = VoiceMsg.toVelocity 64
   in  EventList.fromPairList $
         (0, Event.MetaEvent (MetaEvent.TrackName (show instrument))):
         (0, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.ProgramChange (instrumentToProgram instrument))))):
         (tcPatternToMidiEventTransformer (pt) (Arc 0 (scoreDuration midiScore)) (tsTimeSignatures midiScore))

mapTCDrumToGeneralMIDI :: (Value, Value) -> Note
mapTCDrumToGeneralMIDI (VS "bd", _) = 36
mapTCDrumToGeneralMIDI (VS "sn", VN 4) = 37
mapTCDrumToGeneralMIDI (VS "sn", _) = 38
mapTCDrumToGeneralMIDI (VS "tom", VN 0) = 43
mapTCDrumToGeneralMIDI (VS "tom", VN 1) = 43
mapTCDrumToGeneralMIDI (VS "tom", VN 2) = 41
mapTCDrumToGeneralMIDI (VS "tom", _) = 43
mapTCDrumToGeneralMIDI (VS "hh", _) = 42
mapTCDrumToGeneralMIDI (VS "hho", _) = 46
mapTCDrumToGeneralMIDI (VS "rd", VN 0) = 51
mapTCDrumToGeneralMIDI (VS "rd", VN 1) = 59
mapTCDrumToGeneralMIDI (VS "rd", VN 2) = 53
mapTCDrumToGeneralMIDI (VS "rd", _) = 51
mapTCDrumToGeneralMIDI (VS "cr", VN 0) = 49
mapTCDrumToGeneralMIDI (VS "cr", VN 1) = 49
mapTCDrumToGeneralMIDI (VS "cr", VN 2) = 57
mapTCDrumToGeneralMIDI (VS "cr", VN 3) = 57
mapTCDrumToGeneralMIDI (VS "cr", _) = 49
mapTCDrumToGeneralMIDI (VS "ch", _) = 52
mapTCDrumToGeneralMIDI (VS "mi", _) = 55
mapTCDrumToGeneralMIDI (_, _) = 0

addDrumMidiNote = withEvents aux
         where aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) $ es)
               steppityIn xs = mapMaybe (\(n, ev) -> (transform ev)) $ enumerate xs
               transform (Event c (Just (Arc s e)) a' v) = return (Event c (Just (Arc s e)) a' (newValueMap v))
               noteValue v = fromMaybe (VN 0) (Data.Map.lookup "n" v)
               soundValue v = fromMaybe (VS "") (Data.Map.lookup "s" v)
               newValueMap x = Data.Map.insert "note" (VN ((mapTCDrumToGeneralMIDI (soundValue x, noteValue x)) - 60 )) x

-- Tidal Drum
midiDrumTrack pt midiScore=
   let chan = ChannelMsg.toChannel 9
       vel  = VoiceMsg.toVelocity 64
   in  EventList.fromPairList $
         (0, Event.MetaEvent (MetaEvent.TrackName ("DrumKit"))):
         (0, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.ProgramChange (drumProgram))))):
         (0, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice $ VoiceMsg.Control (VoiceMsg.toController 101) 0))):
         (0, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice $ VoiceMsg.Control (VoiceMsg.toController 100) 0))):
         (0, Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice $ VoiceMsg.Control (VoiceMsg.toController 6) 6))):
         (tcPatternToMidiEventTransformer (addDrumMidiNote pt) (Arc 0 (scoreDuration midiScore)) (tsTimeSignatures midiScore))

         -- Create time signature track for midiFile
calculateTimeSignatures:: (RealFrac b, Integral a) => [(b, b, Int)] -> [(a, Event.T)]
calculateTimeSignatures [] = []
calculateTimeSignatures (x:[]) = createSignatureEvent (x) : (calculateTimeSignatures [])
calculateTimeSignatures ((a1,b1,c1):(a2,b2,c2):xs) = (createSignatureEvent (a1 - a2, b1, c1)) : (calculateTimeSignatures ((a2,b2,c2):xs))
  where
    createSignatureEvent :: (RealFrac a1, Integral a2) => (a1, a1, Int) -> (a2, Event.T)
    createSignatureEvent (a, b, c) = (round ((480 * (b / (2 ^^ c)) * 4) * a), Event.MetaEvent (MetaEvent.TimeSig (round b) c 24 8))

createSignatureEvent :: (RealFrac a1, Integral a2) => (a1, a1, Int) -> (a2, Event.T)
createSignatureEvent (a, b, c) = (round ((480 * (b / (2 ^^ c)) * 4) * a), Event.MetaEvent (MetaEvent.TimeSig (round b) c 24 8))

shiftTimeSignatures :: Num a => [(a, b)] -> [(a, b)]
shiftTimeSignatures [] = []  -- Handle empty list case
shiftTimeSignatures ((x, event1):rest) = (0, event1) : shiftRest x rest
  where
    shiftRest _ [] = []
    shiftRest prevTicks ((ticks, event):xs) = (prevTicks, event) : shiftRest ticks xs

midiFile ::  TidalMIDIScore -> [MidiFile.Track] -> MidiFile.T
midiFile midiScore tracks =
   let createTimeSignatureEvents ts= shiftTimeSignatures $ reverse $ calculateTimeSignatures (reverse ts)
   in  MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks tick) $
    [EventList.fromPairList $
      (0, Event.MetaEvent (MetaEvent.SetTempo (round ((1.0 / ( globalBPM midiScore)) * 60000000)))) :
      (0, Event.MetaEvent (MetaEvent.TrackName (songName midiScore) ) ) :
      (createTimeSignatureEvents (tsTimeSignatures midiScore))
      ] ++ tracks
