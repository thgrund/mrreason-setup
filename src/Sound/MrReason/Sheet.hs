{-# LANGUAGE OverloadedStrings #-}

module Sound.MrReason.Sheet (Sheet (..), sheet, prog) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Function (on)
import Data.List (
  drop,
  groupBy,
  intercalate,
  length,
  sort,
  sortBy,
  transpose,
 )
import Data.Map (
  Map,
  delete,
  findWithDefault,
  fromList,
  insert,
  lookup,
  member,
  (!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Base (ord)
import GHC.Unicode (isDigit)
import Sound.Tidal.Context hiding (scale)
import Sound.Tidal.Core
import Sound.Tidal.Pattern
import Sound.Tidal.Utils
import Prelude hiding ((*>), (<*))
import Sound.MrReason.Numeral (Numeral (Numeral), Modifier (..))

scale =
  getScale
    ( scaleTable
        ++ [ ("H1", [0, 2, 3, 5, 7, 8, 11])
           , ("H2", [0, 1, 3, 5, 6, 9, 10])
           , ("H3", [0, 2, 4, 5, 8, 9, 11])
           , ("H4", [0, 2, 3, 6, 7, 9, 10])
           , ("H5", [0, 1, 4, 5, 7, 8, 10])
           , ("H6", [0, 3, 4, 6, 7, 9, 11])
           , ("H7", [0, 1, 3, 4, 6, 8, 9])
           ]
    )

-- Used for western music theory
-- Sheet datatype
data Sheet a = Sheet
  { key :: Pattern Note
  , mode :: Pattern String
  , numerals :: Pattern Numeral
  , functions :: Pattern a -> Pattern a
  }

sheet =
  Sheet
    { key = "c"
    , mode = "major"
    , numerals = "1"
    , functions = id
    }

numeralPat :: (Int, b) -> Int
numeralPat (x, _) = x

identifierPat :: (a, String) -> String
identifierPat (_, x) = x

parseNumerals :: String -> (Int, String)
parseNumerals st
  | null st   = (0, "")
  | ':' `elem` st =
      let (left, right) = splitString st
      in (toNum left, right)
  | Prelude.all isNumChar st =
      (toNum st, "")
  | otherwise =
      let (numPart, rest) = span isNumCharOrMinus st
      in (toNum numPart, rest)
  where
    splitString str = case break (== ':') str of
      (left, ':' : right) -> (left, right)
      (left, _)           -> (left, "")
    toNum s = case reads s :: [(Int, String)] of
      [(n, "")] -> n
      _         -> 0
    isNumChar c = isDigit c || c == '-'
    -- allow only one leading '-' in numeric part
    isNumCharOrMinus c = isDigit c || c == '-'

-- key mode numerals identifier
identifierCalcWith :: Note -> String -> Numeral -> Pattern Int -> ControlPattern
identifierCalcWith key md numeral pt = (numeralParse numeral)
 where
  -- numeralTuple = parseNumerals (trnm)
  -- ident = identifierPat numeralTuple
  -- nm = numeralPat numeralTuple
  numeralParse (Numeral r []) = overrideScaleKeepNumerals (pure md) r
  -- numeralParse ('H' : x : xs) | isDigit x = overrideScale' (customScaleMode ((ord x - ord '0') - 1) [0, 2, 3, 5, 7, 8, 11])
  -- numeralParse (Numeral r (Locrian : xs)) = overrideScale "locrian" r
  -- numeralParse ('0' : xs) = note (scale (pure md) ((|+|) ((|+|) (pure nm) (-2)) pt) |+ (pure key))
  numeralParse (Numeral r (HarmonicMode1 : xs)) = harmonicScale '1' r
  numeralParse (Numeral r (HarmonicMode2 : xs)) = harmonicScale '2' r
  numeralParse (Numeral r (HarmonicMode3 : xs)) = harmonicScale '3' r
  numeralParse (Numeral r (HarmonicMode4 : xs)) = harmonicScale '4' r
  numeralParse (Numeral r (HarmonicMode5 : xs)) = harmonicScale '5' r
  numeralParse (Numeral r (HarmonicMode6 : xs)) = harmonicScale '6' r
  numeralParse (Numeral r (HarmonicMode7 : xs)) = harmonicScale '7' r
  numeralParse (Numeral r (Plus : xs)) = note 1 + (numeralParse (Numeral r xs))
  numeralParse (Numeral r (Minus : xs)) = (numeralParse (Numeral r xs)) - note 1
  numeralParse (Numeral r (TritonusSubstitutionUp : xs)) = (numeralParse (Numeral r xs)) + note 6
  numeralParse (Numeral r (TritonusSubstitutionDown : xs)) = (numeralParse (Numeral r xs)) - note 6
  numeralParse (Numeral r (OctaveUp : xs)) = (numeralParse (Numeral r xs)) + note 12
  numeralParse (Numeral r (OctaveDown : xs)) = (numeralParse (Numeral r xs)) - note 12
  numeralParse (Numeral r (SecondaryDominantUp : xs)) = (note (scale (pure "major") (pt |+ 3)) |- note 7) |+| (note $ scale (pure "major") ((pure r) |- 1) |+ (pure key) |+ 7)
  numeralParse (Numeral r (SecondaryDominantDown : xs)) = ((note (scale (pure "major") (pt |+ 3)) |- note 7) |+| (note $ scale (pure "major") ((pure r) |- 1) |+ (pure key) |+ 7)) |- note 12
  numeralParse (Numeral r (Diminished : xs)) = overrideScale "diminished" r
  numeralParse (Numeral r (Augmented : xs)) = overrideScale "augmented" r
  numeralParse (Numeral r (Ionian : xs)) = overrideScaleKeepNumerals "ionian" r
  numeralParse (Numeral r (Dorian : xs)) = overrideScaleKeepNumerals "dorian" r
  numeralParse (Numeral r (Phrygian : xs)) = overrideScaleKeepNumerals "phrygian" r
  numeralParse (Numeral r (Lydian : xs)) = overrideScaleKeepNumerals "lydian" r
  numeralParse (Numeral r (Mixolydian : xs)) = overrideScaleKeepNumerals "mixolydian" r
  numeralParse (Numeral r (Aeolian : xs)) = overrideScaleKeepNumerals "aeolian" r
  numeralParse (Numeral r (HarmonicMinor : xs)) = overrideScaleKeepNumerals "harmonicMinor" r
  numeralParse (Numeral r (x : xs)) = numeralParse (Numeral r xs)
  overrideScale sc r = note (scale (pure sc) (pt |- 1)) |+ note (pure key) |+| note (scale (pure md) ((pure r))) |- note 2
  overrideScale' l r = note (toScale l (pt |- 1)) |+ note (pure key) |+| note (scale (pure md) ((pure r))) |- note 2
  harmonicScale x r = overrideScale' (customScaleMode ((ord x - ord '0') - 1) [0, 2, 3, 5, 7, 8, 11]) r
  overrideScaleKeepNumerals sc r = note (scale (sc) ((|+|) ((|+|) (pure r) (-2)) pt) |+ (pure key))
  customScaleMode x ds = map (subtract $ head calcList) $ calcList where calcList = Data.List.drop x $ take ((Data.List.length ds) + x) $ concatMap (\x -> map (+ x) ds) [0, 12 ..]

isOffsetIdentifier :: Char -> Bool
isOffsetIdentifier c = c `elem` ("toOpm" :: String)

compareIdent :: Char -> Char -> Ordering
compareIdent a b
  | isOffsetIdentifier a && isOffsetIdentifier b || not (isOffsetIdentifier a) && not (isOffsetIdentifier b) = EQ
  | isOffsetIdentifier a && not (isOffsetIdentifier b) = LT
  | otherwise = GT

identifierCalc :: Pattern Note -> Pattern String -> Pattern Numeral -> Pattern Int -> ControlPattern
identifierCalc key md nm pt = patternify3 identifierCalcWith (segment 1 $ key) (segment 1 $ md) (segment 1 $ nm) pt

prog :: Sheet ValueMap -> Pattern Int -> ControlPattern
prog sheet pt = (functions sheet) $ identifierCalc (key sheet) (mode sheet) (numerals sheet) pt

