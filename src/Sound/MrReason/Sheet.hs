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
  , numerals :: Pattern String
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
identifierCalcWith :: Note -> String -> String -> Pattern Int -> ControlPattern
identifierCalcWith key md trnm pt = (numeralParse ident)
 where
  numeralTuple = parseNumerals (trnm)
  ident = identifierPat numeralTuple
  nm = numeralPat numeralTuple
  numeralParse ([]) = overrideScaleKeepNumerals (pure md)
  numeralParse ('H' : x : xs) | isDigit x = overrideScale' (customScaleMode ((ord x - ord '0') - 1) [0, 2, 3, 5, 7, 8, 11])
  numeralParse ('l' : xs) = overrideScale "locrian"
  numeralParse ('0' : xs) = note (scale (pure md) ((|+|) ((|+|) (pure nm) (-2)) pt) |+ (pure key))
  numeralParse ('p' : xs) = note 1 + (numeralParse xs)
  numeralParse ('O' : xs) = note 12 + (numeralParse xs)
  numeralParse ('m' : xs) = (numeralParse xs) - note 1
  numeralParse ('t' : xs) = (numeralParse xs) - note 6
  numeralParse ('T' : xs) = (numeralParse xs) + note 6
  numeralParse ('o' : xs) = (numeralParse xs) - note 12
  numeralParse ('S' : xs) = (note (scale (pure "major") (pt |+ 3)) |- note 7) |+| (note $ scale (pure "major") ((pure nm) |- 1) |+ (pure key) |+ 7)
  numeralParse ('s' : xs) = ((note (scale (pure "major") (pt |+ 3)) |- note 7) |+| (note $ scale (pure "major") ((pure nm) |- 1) |+ (pure key) |+ 7)) |- note 12
  -- numeralParse ('d':xs) = overrideScale "locrian"
  numeralParse ('d' : xs) = overrideScale "diminished"
  numeralParse ('a' : xs) = overrideScale "augmented"
  numeralParse ('I' : xs) = overrideScaleKeepNumerals "ionian"
  numeralParse ('D' : xs) = overrideScaleKeepNumerals "dorian"
  numeralParse ('P' : xs) = overrideScaleKeepNumerals "phrygian"
  numeralParse ('L' : xs) = overrideScaleKeepNumerals "lydian"
  numeralParse ('M' : xs) = overrideScaleKeepNumerals "mixolydian"
  numeralParse ('A' : xs) = overrideScaleKeepNumerals "aeolian"
  numeralParse ('H' : xs) = overrideScaleKeepNumerals "harmonicMinor"
  numeralParse (x : xs) = numeralParse $ xs
  overrideScale sc = note (scale (pure sc) (pt |- 1)) |+ note (pure key) |+| note (scale (pure md) ((pure nm))) |- note 2
  overrideScale' l = note (toScale l (pt |- 1)) |+ note (pure key) |+| note (scale (pure md) ((pure nm))) |- note 2
  overrideScaleKeepNumerals sc = note (scale (sc) ((|+|) ((|+|) (pure nm) (-2)) pt) |+ (pure key))
  customScaleMode x ds = map (subtract $ head calcList) $ calcList where calcList = Data.List.drop x $ take ((Data.List.length ds) + x) $ concatMap (\x -> map (+ x) ds) [0, 12 ..]

isOffsetIdentifier :: Char -> Bool
isOffsetIdentifier c = c `elem` ("toOpm" :: String)

compareIdent :: Char -> Char -> Ordering
compareIdent a b
  | isOffsetIdentifier a && isOffsetIdentifier b || not (isOffsetIdentifier a) && not (isOffsetIdentifier b) = EQ
  | isOffsetIdentifier a && not (isOffsetIdentifier b) = LT
  | otherwise = GT

identifierCalc :: Pattern Note -> Pattern String -> Pattern String -> Pattern Int -> ControlPattern
identifierCalc key md nm pt = patternify3 identifierCalcWith (segment 1 $ key) (segment 1 $ md) (segment 1 $ nm) pt

prog :: Sheet ValueMap -> Pattern Int -> ControlPattern
prog sheet pt = (functions sheet) $ identifierCalc (key sheet) (mode sheet) (numerals sheet) pt

