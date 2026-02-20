{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.MrReason.SongConfigParser (generateMidi, generateSoundList, sounds, songDuration, songPatterns, ptOffset, extractSongMetadata, SongMetadata (songName, timeSignatures, bpm)) where

import Control.Exception (Exception, throw)
import Control.Monad (when)
import Data.Aeson (
    FromJSON,
    decodeFileStrict,
    parseJSON,
    withObject,
    (.:),
    (.:?),
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Char (digitToInt, ord)
import Data.IORef (readIORef)
import Data.List (groupBy, intercalate, sortOn)
import Data.List.Split (splitOn, splitOneOf)
import Data.Map (
    elems,
    findWithDefault,
    fromList,
    fromListWith,
    keys,
    lookup,
    toList,
    (!),
 )
import Data.Word (Word8)

import Data.Maybe (fromMaybe)
import Data.Set (difference, fromList, toList)
import qualified Data.Set as Set (fromList)
import GHC.Generics (Generic)
import Sound.MIDI.File.Save (toFile)
import Sound.MIDI.General
import Sound.MrReason.Setup (
    allowedAndNeededInstrumentKeys,
    mapfx,
    transformStacker,
    transformStacker',
    ur',
 )
import Sound.Tidal.Context hiding (offset, segment)
import Sound.Tidal.MIDI (
    TidalMIDIScore (..),
    midiDrumTrack,
    midiFile,
    midiNoteTrack,
 )
import Sound.Tidal.ParseBP
import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Time (Time)
import System.IO (hPutStrLn, stderr)

-- Define a custom exception
data SongMetadataException = SongMetadataNotFoundException deriving (Show)

instance Exception SongMetadataException

-- Define the Person data type

data Segment = Segment
    { segment :: (String, String, Int, String, String)
    , offset :: Maybe Int
    }
    deriving (Show, Generic)

data SongMetadata = SongMetadata
    { displayName :: String
    , songName :: String
    , bpm :: Double
    , timeSignatures :: [(Int, Int)]
    , segments :: [Segment]
    }
    deriving (Show, Generic)

instance FromJSON Segment where
    parseJSON = withObject "Segment" $ \v ->
        Segment
            <$> v .: "segment" -- Required field 'segment'
            <*> v .:? "offset" -- Optional field 'offset', using .:? to parse Maybe

instance FromJSON SongMetadata

-- Function to extract or throw an exception
extractMetadata :: Maybe SongMetadata -> SongMetadata
extractMetadata (Just metadata) = metadata
extractMetadata Nothing = throw SongMetadataNotFoundException

--
extractDuration (_, _, c, _, _) = c

extractPattern (_, _, _, d, _) = d

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)

--
-- midiScore {bpm = 138, timeSignatures = [(1 % 1, 3, 2),(2 % 1, 7, 2), (3 % 1, 9 , 3), (4 % 1, 5 , 2)]}
--
mergeTimeSignatures :: Int -> Maybe (Int, Int) -> (Int, Int, Int)
mergeTimeSignatures x (Just (y, z)) = (x, y, z)
mergeTimeSignatures _ Nothing = (1, 4, 2)

--
-- -- Map each element in b to the corresponding tuple in a
mergedData ts trp = map (\(x, y, z) -> mergeTimeSignatures (stringToInt z) (Prelude.lookup (stringToInt y) mergeInformations)) trp
  where
    mergeInformations = (zip [1 ..] ts)
    stringToInt x = foldl (\acc c -> acc * 10 + (ord c) - 48) 0 x

sumAdjacent :: [(Time, Time, Int)] -> [(Time, Time, Int)]
sumAdjacent [] = []
sumAdjacent ((x, y, z) : []) = (x, y, z) : sumAdjacent [] -- If there's only one element, there's no neighbor to compare
sumAdjacent ((x1, y1, z1) : (x2, y2, z2) : xs) = (x1, y1, z1) : (sumAdjacent ((x1 + x2, y2, z2) : xs))

--
filterAdjacent :: [(Time, Time, Int)] -> [(Time, Time, Int)]
filterAdjacent [] = []
filterAdjacent ((x, y, z) : []) = (x, y, z) : filterAdjacent [] -- If there's only one element, there's no neighbor to compare
filterAdjacent ((x1, y1, z1) : (x2, y2, z2) : xs)
    | y1 == y2 && z1 == z2 = (filterAdjacent ((x2, y2, z2) : xs))
    | otherwise = (x1, y1, z1) : (filterAdjacent ((x2, y2, z2) : xs))

mapfxTs ts = map (\(x, y) -> (pure $ ((fromIntegral x) / (2 ^ y))) :: Pattern Double) ts

mapfxBpm bpm = (pure $ (bpm * 0.5) / 120) :: Pattern Double

mapSoundToMidiInstrument "sally" = ElectricGuitarClean
mapSoundToMidiInstrument "lead" = Lead1Square
mapSoundToMidiInstrument "pwm" = Lead3Calliope
mapSoundToMidiInstrument "lgit" = ElectricGuitarClean
mapSoundToMidiInstrument "git" = DistortionGuitar
mapSoundToMidiInstrument "pad" = Pad2Warm
mapSoundToMidiInstrument "bass" = ElectricBassFinger
mapSoundToMidiInstrument "fbass" = ElectricBassFinger
mapSoundToMidiInstrument "saw" = Lead2Sawtooth
mapSoundToMidiInstrument _ = AcousticGrandPiano

word8ToPattern :: [Word8] -> Pattern [Word8]
word8ToPattern v = pure v

toWord8List :: String -> [Word8]
toWord8List = BS.unpack . B.pack

groupByKey :: String -> [Event ValueMap] -> [(String, [Event ValueMap])]
groupByKey key events = Data.Map.toList $ Data.Map.fromListWith (++) [(value' ev, [ev]) | ev <- events]
  where
    value' ev = case Data.Map.lookup key (value ev) of
        Just (VS s) -> s -- Extract the value for the "s" key, assuming it's a VS (string) type
        _ -> "" -- Default to empty string if key not found or value is not a string

songDuration songMetadata = toRational $ foldl (\acc y -> acc + (extractDuration (segment y))) 0 (segments songMetadata)

songPatterns songMetadata = foldl (\acc y -> acc ++ " " ++ (extractPattern (segment y))) "" (segments songMetadata)

ptOffset a = parseBP_E ('<' : (reverse $ '>' : (reverse (rotLPattern a)))) :: Pattern Time
  where
    rotLPattern a = foldl (\acc x -> acc ++ " " ++ (show $ fromMaybe 0 (offset x)) ++ "!" ++ (show $ extractDuration $ segment x)) "" (segments a)

generateSoundList jsonInputPath songSegments = do
    result <- decodeFileStrict jsonInputPath :: IO (Maybe SongMetadata)
    let extractedResult = extractMetadata result
    -- let a = extractSongMetadata "/Users/mrreason/Documents/livecoding/tidal/ep/scd/songs/wretchedAutomaton.json"
    -- result <- a
    let duration = songDuration extractedResult
    let durationPattern = parseBP_E $ songPatterns extractedResult
    let offset' = ptOffset extractedResult
    let duration = songDuration extractedResult
    let transformedSongSegments = map (\x -> (Data.Map.fromList x)) songSegments
    let labels = Data.Set.toList $ Data.Set.fromList $ concat $ foldl (\acc x -> Data.Map.keys x : acc) [] (map (\x -> (Data.Map.fromList x)) songSegments)
    let ts = timeSignatures extractedResult
    let seg' key =
            patternify rotL (ptOffset extractedResult) $
                ur'
                    (pure duration)
                    (durationPattern)
                    ((transformStacker' transformedSongSegments labels) ! key)
                    (mapfx (mapfxBpm (bpm extractedResult)) (mapfxTs ts))
    let transformPatternsToSortedEvents pt dur = map (\(x, y) -> (x, sortOn part y)) $ groupByKey "s" $ queryArc (pt) dur
    let b = concat $ map (\x -> Data.Map.keys (Data.Map.fromList (transformPatternsToSortedEvents (seg' x) (Arc 0 duration)))) labels
    return (word8ToPattern $ toWord8List $ intercalate "," b)

-- Extract labels
--
-- -- Send

generateMidi jsonInputPath midiOutputPath drumList stacker = do
    -- Step 1: Decode the JSON file
    result <- decodeFileStrict jsonInputPath :: IO (Maybe SongMetadata)
    extractedDrumList <- readIORef drumList
    let drumKeys = Data.Set.toList $ Data.Set.fromList $ concatMap (map fst) (Data.Map.elems extractedDrumList)
    -- Step 2: Extract metadata from the result
    let extractedResult = extractMetadata result
    -- Step 3: Calculate the total duration of the song
    let memoriesDuration = songDuration extractedResult
    -- Step 4: Extract the pattern
    let pattern = songPatterns extractedResult
    -- Step 5: Process the pattern into triplets
    let trp = map (tuplify3 . splitOneOf ":@") $ splitOn " " $ tail pattern
    -- Step 6: Extract time signatures
    let ts = timeSignatures extractedResult
    -- Step 7: Merge time signatures and triplets
    let mergedResult = mergedData ts trp
    -- Step 8: Transform and filter the time signatures
    let ts' = filterAdjacent $ sumAdjacent (map (\(x, y, z) -> (toRational x, toRational y, z)) mergedResult)
    -- Step 9: Create the MIDI score
    let a =
            TidalMIDIScore
                { midiSongName = displayName extractedResult
                , tsTimeSignatures = ts'
                , globalBPM = bpm extractedResult
                , urPattern = parseBP_E pattern
                , scoreDuration = memoriesDuration
                }
    let labels = Data.Set.toList $ Data.Set.fromList $ concat $ foldl (\acc x -> Data.Map.keys x : acc) [] (stacker)
    let seg' key =
            patternify rotL (ptOffset extractedResult) $
                ur'
                    (pure $ scoreDuration a)
                    (urPattern a)
                    ((transformStacker' stacker labels) ! key)
                    (mapfx (mapfxBpm (bpm extractedResult)) (mapfxTs ts))
    let
        -- extendPattern "LGit" = fix (# silence) (s "[gitMode, lgitMode]") $ seg' "LGit"
        extendPattern "RGit" = fix (# silence) (s "[gitMode, lgitMode]") $ seg' "RGit"
        extendPattern "Bass" = seg' "Bass" |- note 24
        extendPattern x = seg' x
    let transformPatternsToSortedEvents pt dur = map (\(x, y) -> (x, sortOn part y)) $ groupByKey "s" $ queryArc (pt) dur
    let allowedKeys = Data.Set.toList $ Data.Set.difference (Data.Set.fromList labels) (Data.Set.fromList drumKeys)
    let midiNoteTracks = map (\(x, evs) -> (midiNoteTrack (mapSoundToMidiInstrument x) evs a)) $ concat $ map (\tcTrackName -> transformPatternsToSortedEvents (extendPattern tcTrackName) (Arc 0 (scoreDuration a))) allowedKeys
    let drumTracks a = map (\x -> midiDrumTrack (seg' x) a) drumKeys
    -- Step 11: Write the MIDI file to disk
    toFile
        midiOutputPath
        ( midiFile
            a
            ((drumTracks a) ++ midiNoteTracks)
        )

sounds = pX "sounds"

extractSongMetadata jsonInputPath = do
    result <- decodeFileStrict jsonInputPath :: IO (Maybe SongMetadata)
    return $ extractMetadata result
