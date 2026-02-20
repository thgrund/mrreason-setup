{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.MrReason.OrbitConfigParser (generateStreams, StreamConfig (..), streamConfig) where

import Control.Exception (Exception, throw)
import Data.Aeson (
    FromJSON,
    decodeFileStrict,
    parseJSON,
    withObject,
    (.:),
    (.:?),
 )
import Data.Map (
    findWithDefault,
    fromList,
    (!),
 )
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Sound.MrReason.Setup (
    cP',
    mapfx,
    transformStacker',
    ur',
 )
import Sound.MrReason.SongConfigParser
import Sound.Tidal.Context
import qualified Sound.Tidal.ID as ID

data SuperDirtOrbitMetadata = SuperDirtOrbitMetadata
    {label :: String}
    deriving (Show, Generic)

instance FromJSON SuperDirtOrbitMetadata

data SuperDirtOrbitMetadataException = SuperDirtOrbitMetadataNotFoundException deriving (Show)

instance Exception SuperDirtOrbitMetadataException

extractMetadata :: Maybe [SuperDirtOrbitMetadata] -> [SuperDirtOrbitMetadata]
extractMetadata (Just metadata) = metadata
extractMetadata Nothing = throw SuperDirtOrbitMetadataNotFoundException

-- globalFx == ((tParam rotL) $ cT 0 "offset")
-- part == (cP' "~" "wretchedAutomaton") -> wretchedAutomaton == songName (config)
-- dur == ((cT 0 "duration"))
-- fx ==  (mapfx 0.48 [4/4])  ->  "bpm": 115 "timeSignatures": [[4,2]] (config)
-- ping == "wretchedAutomatonPing" -> songName + Ping
-- stackers -> muss übergeben werden
--

-- songName b ++ "Ping"
-- songName b

-- map (\(x,y) -> ((fromIntegral x) / (2 ** ( fromIntegral y ))) ) (timeSignatures b)

-- (bpm b / 120) / 2

shallLoadConfig = pB "shallLoadConfig"

shallUIBeUpdated = pB "shallUIBeUpdated"

shallLoadPreset = pB "shallLoadPreset"

data StreamConfig = StreamConfig
    { streamLoadConfig :: Pattern Bool
    , streamLoadPreset :: Pattern Bool
    , streamConfigUIUpdate :: Pattern Bool
    , ping :: Pattern Time
    }

streamConfig =
    StreamConfig
        { streamLoadConfig = "f"
        , streamLoadPreset = "f"
        , streamConfigUIUpdate = "t"
        , ping = 0.04
        }

-- generateStreams "/Users/mrreason/Development/SuperCollider/SuperDirtMixer/presets/Factory.json" (cP' "~" "wretchedAutomaton") (mapfx 0.48 [4/4]) "wretchedAutomatonPing" p x

generateStreams orbitJsonInputPath songJsonInputPath p conf stackers = do
    orbitJsonResult <- decodeFileStrict orbitJsonInputPath :: IO (Maybe [SuperDirtOrbitMetadata])
    songJsonResult <- extractSongMetadata songJsonInputPath
    let extractedMetadata = extractMetadata orbitJsonResult
        labels = map (\x -> (label x)) extractedMetadata
        labelsWithNumber = zip [0 .. length labels] labels
        parts = transformStacker' (map (\x -> (Data.Map.fromList x)) stackers) labels
        -- parts = transformStacker' stackers labels
        transformedTimeSignatures = map (\(x, y) -> ((fromIntegral x) / (2 ** (fromIntegral y)))) (timeSignatures songJsonResult)
        transformedBpms = ((bpm songJsonResult) / 120) / 2
        seg key = slow (cP' "1" "loopDuration")
                $ zoompat (cP' "0" "loopStart") (cP' "1" "loopEnd")
                $ fast (cP' "1" "loopTotalDuration")
                $ ur' ((cT 0 "duration")) ((cP' "~" (songName songJsonResult))) (parts ! key) (mapfx (pure transformedBpms) transformedTimeSignatures)
        controlOffset = ((patternify rotL) $ cP' "0" "offset")
        globalControlOffset = ((patternify rotL) $ cP' "0" "globalOffset")
    mapM_ (\(i, name) -> p (ID.ID{ID.fromID = (show (toInteger ( i + 1)))}) . (|< orbit (pure i)) . (|< shallUIBeUpdated (streamConfigUIUpdate conf)) $ controlOffset $ globalControlOffset $ (seg name)) labelsWithNumber
    p "ping" . (|< orbit 31) $ (ping conf) <~ s (pure (songName songJsonResult ++ "Ping")) # shallLoadConfig (streamLoadConfig conf) # shallLoadPreset (streamLoadPreset conf)

-- p 6 . (|< orbit 5) $ s (pure (songName songJsonResult ++ "Ping")) # shallLoadConfig (streamConfigLoadPreset conf)
