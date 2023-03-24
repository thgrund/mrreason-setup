
{-# LANGUAGE OverloadedStrings #-}

module MrReason where

import Prelude hiding ((<*), (*>))
import Sound.Tidal.Context

import Data.Function (on) 
import Data.List (transpose, sortBy, sort, intercalate, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)
import Data.Map (findWithDefault, insert, lookup, (!), fromList, member)
import GHC.Base (ord)
import GHC.Unicode (isDigit)

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.Utils (enumerate)

-- Used for western music theory
-- Sheet datatype
data Sheet a = Sheet
  { key :: Pattern Note
  , mode :: Pattern String
  , numerals :: String
  , functions :: Pattern a -> Pattern a
  }

--
-- Used for using parts and segments
transformStacker parts = Data.Map.fromList $ map (\x -> (fst $ head x, transform x)) (transpose $ filledWithSilence parts)
    where allowedAndNeededKeys = ["pure", "lead", "bass", "key", "drums", "pad", "clock", "pod", "arp", "fx", "yamaha", "rhytm", "rhytmGit", "leadGit"]
          filledWithSilence parts = map (\x -> addSilence x) parts
          addSilence pt = map (\x -> if (member x (pt)) then (x, pt ! x) else (x, silence)) allowedAndNeededKeys
          transform x = transformBy 1 x
          transformBy _ [] = []
          transformBy y [x] = [(show y, snd x)]
          transformBy y (x:xs) = (show y, snd x) : (transformBy (y + 1) xs)

-- Permanent filter transition
lpfBy x d t = ( cutoff (15000 - (15000 - d) * (rotR t $ slow x envL)))
lpf' d t = lpfBy 4 d t
hpfBy x s d t = ( hpf (s + d * (rotR t $ slow x envL)))
hpf' d t = hpfBy 4 0 d t

-- Timesignature
mapfxBy _ _ [] = []
mapfxBy y c [x] = [(show y, (# cps (c * (1/x))))]
mapfxBy y c (x:xs) = (show y, (# cps (c * (1/x))) ) : (mapfxBy (y + 1) c xs)
mapfx = mapfxBy 1

drumBank = pI "drumBank"

stacker = Data.Map.fromList

dx st fx = 
    do
        let
            parts = transformStacker [
                stacker st
                ]
            seg orb key = orbit orb <| ur 32 ("1:1")  (parts ! key) fx
        stack [
          seg 0 "lead" 
          , seg 1 "bass"
          , seg 2 "key"
          , seg 3 "pad"
          , seg 4 "arp"
          , fix (# gain 0.8) (s "hho")
            $ fix (# gain 0.9) (s "cr")
            $ fix (# gain (((rand * 0.04) + 0.96) - 0.05)) (s "hh")
            $ fix (# gain (((rand * 0.04) + 0.95) - 0.05)) (s "ch")
            $ fix (# gain 1.2) (s "sn")
            $ fix (# gain 1.5) (s "bd")
            $ seg 5 "drums" # drumBank 2 # gain (((rand * 0.04) + 0.96) - 0.02) # legato 4
          , seg 6 "rhytm" 
          , seg 7 "leadGit"
          , seg 8 "rhytmGit"
          , seg 9 "yamaha"
          , seg 10 "fx"]

-- Helper (add VN to getN)
getN' :: Value -> Maybe Note
getN' (VN f) = Just $ f
getN' _  = Nothing

compareNoteEv (Event c1 t1 a1 v1) (Event c2 t2 a2 v2)
  | Data.Map.lookup "note" v1 == Data.Map.lookup "note" v2 = EQ
  | Data.Map.lookup "note" v1 <= Data.Map.lookup "note" v2 = LT
  | otherwise = GT

-- Generic
noteMap mapperfunc noteVal = ((fmap (unNote . (mapperfunc) ) $ ((filteredNotes noteVal))))
      where filteredNotes notes = filterJust $ fmap (getN') 
                                             $ filterJust 
                                             $ fmap ( Data.Map.lookup "note") (notes)

-- linexp
linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp inMin inMax outMin outMax n
 | inMin > inMax = 1
 | n > inMax = outMax
 | n < inMin = outMin
 | outMin > outMax = (outMin - calc) + outMax
 | otherwise = calc
 where pitch = n + 1 - inMin
       calc = ((logBase (inMax - inMin + 1) pitch) * (outMax-outMin)) + (outMin)

linlin :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linlin inMin inMax outMin outMax n
 | inMin > inMax = 1
 | n > inMax = outMax
 | n < inMin = outMin
 | otherwise = calc
 where pitch = n
       calc = (((pitch - inMin) / (inMax - inMin)) * (outMax - outMin)) + outMin

-- Exp Macros
ngain pt = pt # (gain $ noteMap (linexp 12 36 0.6 1) pt)

ncutoff pt = pt # (cutoff $ noteMap (linlin 0 32 4000 500) pt)

invWith :: Int -> Pattern ValueMap -> Pattern ValueMap
invWith y = withEvents aux
    where aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
          steppityIn x = mapMaybe (\(n, ev) -> return ev) 
            $ enumerate $ sortBy (compareNoteEv) (inv (replicate (abs y) ((applyFunc y negate) (Note 12) )) ((applyFunc y reverse) (sortBy (compareNoteEv) x )))
          applyFunc y f= if y < 0 then f else id
          inv _ []          = []
          inv [] x          = x
          inv (y:ys) ((Event c t a v):xs) = inv ys ((applyFunc y reverse) (sortBy compareNoteEv (Event c t a (insert "note" (add y v) v ):xs)))
          add y x = VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" x) + y

inverse :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap 
inverse pt = tParam invWith (segment 1 $ pt)

openWith :: Bool -> Pattern ValueMap -> Pattern ValueMap
openWith y = withEvents aux
    where aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
          steppityIn x = mapMaybe (\(n, ev) -> return ev) 
            $ enumerate (sortBy (compareNoteEv) $ if (y) then (open x) else x)
          open (xs:[]) = [xs]
          open (xs:ys:[]) = [xs,ys]
          open ((Event c1 t1 a1 v1):ys:(Event c2 t2 a2 v2):x) 
            = (Event c1 t1 a1 (sub v1)) : (Event c2 t2 a2 (sub v2)) : ys : x 
          sub m = insert "note" (VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" m) - 12) m

open :: Pattern Bool -> Pattern ValueMap -> Pattern ValueMap 
open pt = tParam openWith (segment 1 $ pt)

dropWith :: String -> Pattern ValueMap -> Pattern ValueMap
dropWith y = withEvents aux
    where aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
          steppityIn x = mapMaybe (\(n, ev) -> return ev)  $ enumerate (drop y (reverse x))
          drop "0" (xs) = reverse $ xs
          drop "2" (xs:(Event c t a v):x) = reverse $ xs:x ++ [(Event c t a (sub v))]
          drop "3" (xs:ys:(Event c t a v):x) = reverse $ xs:ys:x ++ [(Event c t a (sub v))]
          drop "2p3" (xs:(Event c1 t1 a1 v1):(Event c2 t2 a2 v2):x) = reverse $ xs:x ++ (Event c1 t1 a1 (sub v1)):(Event c2 t2 a2 (sub v2)):[]
          drop "4" (ws:xs:ys:(Event c t a v):x) = reverse $ ws:xs:ys:x ++ [(Event c t a (sub v))]
          drop "2p4" (ws:(Event c1 t1 a1 v1):ys:(Event c2 t2 a2 v2):x) = reverse $ ws:ys:x ++ (Event c1 t1 a1 (sub v1)):(Event c2 t2 a2 (sub v2)):[]
          drop _ x = reverse x 
          sub m = insert "note" (VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" m) - 12) m

drop :: Pattern String -> Pattern ValueMap -> Pattern ValueMap
drop pt = tParam dropWith (segment 1 $ pt)

updown = fastspread ($) [id, rev]
updownBy t p = fast t $ spread ($) [id, rotR 1] $ spread ($) [id, rev] $ slow t $ p
updown' p = updownBy 2 p

downup = fastspread ($) [rev, id]
downupBy t p = fast t $ spread ($) [rotR 1, id] $ spread ($) [id, rev] $ slow t $ p
downup' p = downupBy 2 p

toTuple :: [String] -> (Pattern Int, Pattern String)
toTuple [a,b] = (parseBP_E a,parseBP_E b)

numeralPat :: (Pattern Int, b) -> Pattern Int
numeralPat (x, _) = x

identifierPat :: (a, Pattern String) -> Pattern String 
identifierPat (_, x) = x

filter' :: (Bool -> Bool) -> String -> String
filter' f st =  filter (\x -> f $ isDigit x) st

filterNumerals :: String -> String
filterNumerals st = (
  if st == [] then "" else 
    if (head st == '-') then "-" else "") 
      ++ (filter' id st)

filterIdentifier :: String -> String
filterIdentifier st = (
  if filtered == [] then "" else 
    if (head filtered == '-') then (tail filtered) else filtered)
  where filtered = (filter' not st)

fillEmpty :: String -> String
fillEmpty st = if st == [] then "0" else 
    map (repl) $ st 
    where repl '+' = 'p'
          repl '-'   = 'm'
          repl c = c

-- Start prog
tParam4 :: (a -> b -> c -> d -> e -> Pattern f) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> e -> Pattern f)
tParam4 f a b c d p = innerJoin $ (\w x y z -> f w x y z p) <$> a <*> b <*> c <*> d

-- key mode numerals identifier
identifierCalcWith4 :: Note -> String ->  Int -> String -> Pattern Int -> ControlPattern
identifierCalcWith4 key md nm ident pt = (numeralParse ident)
    where numeralParse ([]) = overrideScaleKeepNumerals (pure md) 
          numeralParse ('0':xs) = note (scale (pure md) ( (|+|) ((|+|) (pure nm) (-2)) pt) |+ (pure key))
          numeralParse ('p':xs) = note 1 + (numeralParse xs)
          numeralParse ('O':xs) = note 12 + (numeralParse xs)
          numeralParse ('m':xs) = (numeralParse xs) - note 1 
          numeralParse ('t':xs) = (numeralParse xs) - note 6
          numeralParse ('o':xs) = (numeralParse xs) - note 12
          numeralParse ('s':xs) = (note (scale (pure "major") (pt |+ 3)) |- note 7 ) |+| (note $ scale (pure "major") ((pure nm) |- 1) |+ (pure key) |+ 7)
          numeralParse ('d':xs) = overrideScale "diminished" 
          numeralParse ('a':xs) = overrideScale "augmented" 
          numeralParse ('I':xs) = overrideScaleKeepNumerals "ionian" 
          numeralParse ('D':xs) = overrideScaleKeepNumerals "dorian"  
          numeralParse ('P':xs) = overrideScaleKeepNumerals "phrygian" 
          numeralParse ('L':xs) = overrideScaleKeepNumerals "lydian" 
          numeralParse ('M':xs) = overrideScaleKeepNumerals "mixolydian" 
          numeralParse ('A':xs) = overrideScaleKeepNumerals "aeolian" 
          numeralParse (x:xs) = numeralParse $ xs
          overrideScale sc = note (scale (pure sc) (pt |- 1)) |+ note (pure key) |+| note (scale (pure md) ((pure nm))) |- note 2 
          overrideScaleKeepNumerals sc = note (scale (sc) ( (|+|) ((|+|) (pure nm) (-2)) pt) |+ (pure key))

isOffsetIdentifier :: Char -> Bool
isOffsetIdentifier c = c `elem` ("+-toOpm" :: String)

compareIdent :: Char -> Char -> Ordering
compareIdent a b
  | isOffsetIdentifier a && isOffsetIdentifier b || not (isOffsetIdentifier a) && not (isOffsetIdentifier b) = EQ
  | isOffsetIdentifier a && not (isOffsetIdentifier b) = LT
  | otherwise = GT

identifierCalc4 :: Pattern Note -> Pattern String -> Pattern Int -> Pattern String -> Pattern Int -> ControlPattern
identifierCalc4 key md nm ident pt = tParam4 identifierCalcWith4 (segment 1 $ key) (segment 1 $ md) (segment 1 $ nm) (segment 1 $ ident) pt

prog :: Sheet ValueMap -> Pattern Int -> ControlPattern
prog sheet pt= (functions sheet) $ identifierCalc4 (key sheet) (mode sheet) (numeralPat numeralsTuple) (identifierPat numeralsTuple) pt
    where delimeter = 43:45:35:[48..57] ++ [65..90] ++ [97..122]
          splittedPattern = foldMap (groupBy (\a b -> ord a `elem` delimeter)) 
            $ groupBy (\a b -> ord b `elem` delimeter) (numerals sheet)
          --
          filteredNumerals = map (filterNumerals) splittedPattern
          --
          filteredIdentifier = map (sortBy (compareIdent)) 
            $ map (\x -> 
              if (x == []) then "" else 
                if (head x == '#') then (tail x) else "") 
                  $ map (filterIdentifier) splittedPattern
          --
          numeralsTuple = toTuple $ map (intercalate " ") 
            $ transpose 
              $ zipWith3 (\x y z -> 
                if (not $ y == []) then y:(fillEmpty z):[] else x:x:[] ) 
                  splittedPattern filteredNumerals filteredIdentifier 
