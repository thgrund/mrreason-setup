{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric      #-}

module Sound.MrReason.ConfigParser (generateMidi) where

import Data.Aeson (FromJSON, decodeFileStrict, parseJSON, (.:), (.:?), withObject)
import Data.Map (lookup, (!), fromListWith, findWithDefault, fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt, ord)
import GHC.Generics (Generic)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import Data.List.Split (splitOneOf, splitOn)
import Data.List (sortOn, groupBy)

import Sound.Tidal.Context hiding (segment, offset)
import Sound.Tidal.Time (Time)
import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.ParseBP

import Control.Exception (throw, Exception)

import Sound.Tidal.MIDI (TidalMIDIScore (..), midiNoteTrack, midiDrumTrack, midiFile)
import Sound.MrReason.Setup (ur', transformStacker, mapfx, allowedAndNeededInstrumentKeys)

import Sound.MIDI.General
import Sound.MIDI.File.Save (toFile)

-- Define a custom exception
data SongMetadataException = SongMetadataNotFoundException deriving (Show)

instance Exception SongMetadataException

-- Define the Person data type

data Segment = Segment
  {
    segment :: (String, String, Int, String, String),
    offset :: Maybe Int
  } deriving (Show, Generic)

data SongMetadata = SongMetadata
  {
    displayName :: String,
    bpm :: Double,
    timeSignatures :: [(Int,Int)],
    segments :: [Segment]
  } deriving (Show, Generic)

instance FromJSON Segment where
  parseJSON = withObject "Segment" $ \v -> Segment
    <$> v .: "segment"        -- Required field 'segment'
    <*> v .:? "offset"        -- Optional field 'offset', using .:? to parse Maybe

instance FromJSON SongMetadata

-- Function to extract or throw an exception
extractMetadata :: Maybe SongMetadata -> SongMetadata
extractMetadata (Just metadata) = metadata
extractMetadata Nothing = throw SongMetadataNotFoundException
--
extractDuration (_,_,c,_,_) = c
extractPattern (_,_,_,d,_) = d

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
--
-- midiScore {bpm = 138, timeSignatures = [(1 % 1, 3, 2),(2 % 1, 7, 2), (3 % 1, 9 , 3), (4 % 1, 5 , 2)]}
--
mergeTimeSignatures :: Int -> Maybe (Int, Int) -> (Int, Int, Int)
mergeTimeSignatures x (Just (y, z)) = (x, y,z)
mergeTimeSignatures _ Nothing = (1, 4,2)
--
-- -- Map each element in b to the corresponding tuple in a
mergedData ts trp = map (\(x, y, z) -> mergeTimeSignatures (stringToInt z) (Prelude.lookup (stringToInt y) mergeInformations)) trp
  where mergeInformations = (zip [1..] ts)
        stringToInt x = foldl (\acc c -> acc * 10 + (ord c) - 48) 0 x

sumAdjacent :: [(Time, Time, Int)] -> [(Time, Time, Int)]
sumAdjacent [] = []
sumAdjacent ((x,y,z):[]) = (x,y,z) : sumAdjacent []  -- If there's only one element, there's no neighbor to compare
sumAdjacent ((x1, y1, z1):(x2, y2, z2):xs) = (x1, y1, z1): (sumAdjacent ((x1 + x2, y2, z2):xs))
--
filterAdjacent :: [(Time, Time, Int)] -> [(Time, Time, Int)]
filterAdjacent [] = []
filterAdjacent ((x,y,z):[]) = (x,y,z) : filterAdjacent []  -- If there's only one element, there's no neighbor to compare
filterAdjacent ((x1, y1, z1):(x2, y2, z2):xs)
  | y1 == y2 && z1 == z2 = (filterAdjacent ((x2, y2, z2):xs))
  | otherwise = (x1, y1, z1) : (filterAdjacent ((x2, y2, z2):xs))

mapfxTs ts = map (\ (x,y) -> (pure $ ((fromIntegral x) / (2 ^ y)))::Pattern Double ) ts
mapfxBpm bpm = (pure $ (bpm * 0.5) / 120) :: Pattern Double

mapSoundToMidiInstrument "sally" = Banjo
mapSoundToMidiInstrument "lead" = Lead1Square
mapSoundToMidiInstrument "pwm" = Lead3Calliope
mapSoundToMidiInstrument "lGit" = ElectricGuitarClean
mapSoundToMidiInstrument "git" = DistortionGuitar
mapSoundToMidiInstrument "pad" = Pad2Warm
mapSoundToMidiInstrument "bass" = ElectricBassFinger
mapSoundToMidiInstrument "fbass" = ElectricBassFinger
mapSoundToMidiInstrument "saw" = Lead2Sawtooth
mapSoundToMidiInstrument _ = AcousticGrandPiano

groupByKey :: String -> [Event ValueMap] -> [(String, [Event ValueMap])]
groupByKey key events = Data.Map.toList $ Data.Map.fromListWith (++) [(value' ev, [ev]) | ev <- events]
  where
    value' ev = case Data.Map.lookup key (value ev) of
      Just (VS s) -> s         -- Extract the value for the "s" key, assuming it's a VS (string) type
      _           -> ""        -- Default to empty string if key not found or value is not a string

generateMidi jsonInputPath midiOutputPath stacker = do
    -- Step 1: Decode the JSON file
    result <- decodeFileStrict jsonInputPath :: IO (Maybe SongMetadata)
    -- Step 2: Extract metadata from the result
    let extractedResult = extractMetadata result
    -- Step 3: Calculate the total duration of the song
    let memoriesDuration = toRational $ foldl (\acc y -> acc + (extractDuration (segment y))) 0 (segments extractedResult)
    -- Step 4: Extract the pattern
    let pattern = foldl (\acc y -> acc ++ " " ++ (extractPattern (segment y))) "" (segments extractedResult)
    -- Step 5: Process the pattern into triplets
    let trp = map (tuplify3 . splitOneOf ":@") $ splitOn " " $ tail pattern
    -- Create offset pattern
    let rotLPattern = foldl (\ acc x -> acc ++ " " ++ (show $ fromMaybe 0 (offset x)) ++ "!" ++ (show $ extractDuration $ segment x) ) "" (segments extractedResult)
    let ptOffset = parseBP_E('<' : (reverse $ '>' : (reverse rotLPattern))) :: Pattern Time
    -- Step 6: Extract time signatures
    let ts = timeSignatures extractedResult
    -- Step 7: Merge time signatures and triplets
    let mergedResult = mergedData ts trp
    -- Step 8: Transform and filter the time signatures
    let ts' = filterAdjacent $ sumAdjacent (map (\(x, y, z) -> (toRational x, toRational y, z)) mergedResult)
    -- Step 9: Create the MIDI score
    let a = TidalMIDIScore {
        songName = displayName extractedResult,
        tsTimeSignatures = ts',
        globalBPM = bpm extractedResult,
        urPattern = parseBP_E pattern,
        scoreDuration = memoriesDuration
    }
    let seg' key = tParam rotL ptOffset $ ur' (pure $ scoreDuration a) (urPattern a) ((transformStacker stacker) ! key)
                (mapfx (mapfxBpm (bpm extractedResult)) (mapfxTs ts))
    let extendPattern "lGit" = fix (# silence) (s "[gitMode, lgitMode]") $ seg' "lGit"
        extendPattern "rGit" = fix (# silence) (s "[gitMode, lgitMode]") $ seg' "rGit"
        extendPattern "bass" = seg' "bass" |- note 24
        extendPattern x = seg' x
    let transformPatternsToSortedEvents pt dur = map (\(x,y) -> (x, sortOn part y)) $ groupByKey "s" $ queryArc (pt) dur
    let midiNoteTracks = map (\(x, evs) -> ( midiNoteTrack (mapSoundToMidiInstrument x) evs a )) $ concat $ map (\tcTrackName -> transformPatternsToSortedEvents (extendPattern tcTrackName) (Arc 0 (scoreDuration a))) allowedAndNeededInstrumentKeys
    -- Step 11: Write the MIDI file to disk
    toFile midiOutputPath (midiFile a
      ((midiDrumTrack (seg' "drums") a) : midiNoteTracks)
      )
