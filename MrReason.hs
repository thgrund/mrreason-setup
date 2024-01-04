
{-# LANGUAGE OverloadedStrings #-}

module MrReason where

import Prelude hiding ((<*), (*>))
import Sound.Tidal.Context

import Data.Function (on)
import Data.List (transpose, sortBy, sort, intercalate, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)
import Data.Map (findWithDefault, insert, lookup, (!), fromList, member, delete)
import Control.Concurrent.MVar (modifyMVar_)

import GHC.Base (ord)
import GHC.Unicode (isDigit)

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.Utils

import qualified Data.Map.Strict as Map

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
    where allowedAndNeededKeys = ["globalfx","pure", "lead", "bass", "key", "drums", "pad", "clock", "pod", "arp", "fx", "yamaha", "rhytm", "rGit", "lGit", "vocal", "strings"]
          filledWithSilence parts = map (\x -> addSilence x) parts
          addSilence pt = map (\x -> if (member x (pt)) then (x, pt ! x) else (x, silence)) allowedAndNeededKeys
          transform x = transformBy 1 x
          transformBy _ [] = []
          transformBy y [x] = [(show y, snd x)]
          transformBy y (x:xs) = (show y, snd x) : (transformBy (y + 1) xs)

-- Permanent filter transition
-- TODO remember latest filter for the orbit
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

reverb = pF "reverb"

gDelay = pF "gDelay"

-- Helper (add VN to getN)
getN' :: Value -> Maybe Note
getN' (VN f) = Just $ f
getN' _  = Nothing

cP' d s = innerJoin $ parseBP_E <$> _cX d getS s

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
    if ('#' `elem` st) then (fst $ splitString st) else (st))
      where splitString str = case break (== '#') str of
                (left, '#' : right) -> (left, right)
                (left, _)           -> (left, "")

filterIdentifier st = (
  if st == [] then "" else
    if ('#' `elem` st) then ((snd $ splitString st) ++ (handleNumeralsPart st)) else (
      if (containsDigit st) then ("0" ++ (handleNumeralsPart st)) else (st)
      ))
      where splitString str = case break (== '#') str of
                (left, '#' : right) -> (left, right)
                (left, _)           -> (left, "")
            containsDigit = any isDigit
            handleLeadingMinus st = if ((head st) == '-') then (tail st) else st
            handleNumeralsPart st =  dropWhile isDigit (handleLeadingMinus $ fst $ splitString st)

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
    where delimeter = 33:35:42:43:[45..57] ++ [64..90] ++ [97..122]
          splittedPattern = foldMap (groupBy (\a b -> ord a `elem` delimeter))
            $ groupBy (\a b -> ord b `elem` delimeter) (numerals sheet)
          --
          filteredNumerals = map (filterNumerals) splittedPattern
          --
          filteredIdentifier = map (sortBy (compareIdent)) $ map (filterIdentifier) splittedPattern
          --
          numeralsTuple = toTuple $ map (intercalate " ")
            $ transpose
              $ zipWith3 (\x y z ->
                if (not $ y == []) then y:(fillEmpty z):[] else x:x:[] )
                  splittedPattern filteredNumerals filteredIdentifier

ur' :: Pattern Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a
ur' t outer_p ps fs = slow t $ unwrap $ adjust <$> timedValues (getPat . split <$> outer_p)
  where split = wordsBy (==':')
        getPat (s:xs) = (match s, transform xs)
        -- TODO - check this really can't happen..
        getPat _ = error "can't happen?"
        match s = fromMaybe silence $ Prelude.lookup s ps'
        ps' = map (fmap (fast t)) ps
        adjust (a, (p, f)) = f a p
        transform (x:_) a = transform' x a
        transform _ _ = id
        transform' str (Arc s e) p = s `rotR` inside (pure $ 1/(e-s)) (matchF str) p
        matchF str = fromMaybe id $ Prelude.lookup str fs
        timedValues = withEvent (\(Event c (Just a) a' v) -> Event c (Just a) a' (a,v)) . filterDigital

gm = pF "gm"

mode' name pt = 0.01 <~ s name # note pt

-- Generic version for streams
streamUnset :: Stream -> String -> IO ()
streamUnset stream k = modifyMVar_ (sStateMV stream) (return . Map.delete k)

presetFile = pS "presetFile"
