{-# LANGUAGE OverloadedStrings #-}

module Sound.MrReason.Setup (ur', (<:), mergeStreams, mkDrums, mkProgs, getPt, transformProgs, transformDrums, transformStacker, transformStacker', mapfx, mode', mode'', drumBank, enableLiveGit, Sound.MrReason.Setup.drop, inverse, open, cP', songNamePt, allowedAndNeededInstrumentKeys, allowedAndNeededKeys, transposeDrumMap, inhabit') where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Function (on)
import Data.IORef
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
import Text.Read (readMaybe)
import Prelude hiding ((*>), (<*))
import Sound.MrReason.Sheet (Sheet, prog)

allowedAndNeededInstrumentKeys =
  [ "lead"
  , "bass"
  , "key"
  , "pad"
  , "arp"
  , "fx"
  , "rhytm"
  , "rGit"
  , "lGit"
  , "vocal"
  , "strings"
  ]

allowedAndNeededDrumKeys = ["drums"]

allowedAndNeededFunctionalKeys = ["globalfx", "pure", "looper", "loop"]

mergedAndNeededFunctionalKeys = allowedAndNeededInstrumentKeys ++ allowedAndNeededDrumKeys ++ allowedAndNeededFunctionalKeys

allowedAndNeededKeys = mergedAndNeededFunctionalKeys ++ map (\x -> x ++ "Fx") mergedAndNeededFunctionalKeys

--
-- Used for using parts and segments
transformStacker' parts labels = Data.Map.fromList $ map (\x -> (fst $ head x, transform x)) (transpose $ filledWithSilence parts)
 where
  filledWithSilence parts = map (\x -> addSilence x) parts
  addSilence pt = map (\x -> if (Map.member x (pt)) then (x, pt ! x) else (x, silence)) labels
  transform x = transformBy 1 x
  transformBy _ [] = []
  transformBy y [x] = [(show y, snd x)]
  transformBy y (x : xs) = (show y, snd x) : (transformBy (y + 1) xs)

transformStacker parts = transformStacker' parts allowedAndNeededKeys

-- Permanent filter transition
-- TODO remember latest filter for the orbit
lpfBy x d t = (cutoff (15000 - (15000 - d) * (rotR t $ slow x envL)))

lpf' d t = lpfBy 4 d t

hpfBy x s d t = (hpf (s + d * (rotR t $ slow x envL)))

hpf' d t = hpfBy 4 0 d t

-- Timesignature
mapfxBy _ _ [] = []
mapfxBy y c [x] = [(show y, (# cps (c * (1 / x))))]
mapfxBy y c (x : xs) = (show y, (# cps (c * (1 / x)))) : (mapfxBy (y + 1) c xs)

mapfx = mapfxBy 1

drumBank = pI "drumBank"

reverb = pF "reverb"

gDelay = pF "gDelay"

-- Helper (add VN to getN)
getN' :: Value -> Maybe Note
getN' (VN f) = Just $ f
getN' _ = Nothing

compareNoteEv (Event c1 t1 a1 v1) (Event c2 t2 a2 v2)
  | Data.Map.lookup "note" v1 == Data.Map.lookup "note" v2 = EQ
  | Data.Map.lookup "note" v1 <= Data.Map.lookup "note" v2 = LT
  | otherwise = GT

-- Generic
noteMap mapperfunc noteVal = ((fmap (unNote . (mapperfunc)) $ ((filteredNotes noteVal))))
 where
  filteredNotes notes =
    filterJust $
      fmap (getN') $
        filterJust $
          fmap (Data.Map.lookup "note") (notes)

-- linexp
linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linexp inMin inMax outMin outMax n
  | inMin > inMax = 1
  | n > inMax = outMax
  | n < inMin = outMin
  | outMin > outMax = (outMin - calc) + outMax
  | otherwise = calc
 where
  pitch = n + 1 - inMin
  calc = ((logBase (inMax - inMin + 1) pitch) * (outMax - outMin)) + (outMin)

linlin :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
linlin inMin inMax outMin outMax n
  | inMin > inMax = 1
  | n > inMax = outMax
  | n < inMin = outMin
  | otherwise = calc
 where
  pitch = n
  calc = (((pitch - inMin) / (inMax - inMin)) * (outMax - outMin)) + outMin

-- Exp Macros
ngain pt = pt # (gain $ noteMap (linexp 12 36 0.6 1) pt)

ncutoff pt = pt # (cutoff $ noteMap (linlin 0 32 4000 500) pt)

invWith :: Int -> Pattern ValueMap -> Pattern ValueMap
invWith y = withEvents aux
 where
  aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
  steppityIn x =
    mapMaybe (\(n, ev) -> return ev) $
      enumerate $
        sortBy (compareNoteEv) (inv (replicate (abs y) ((applyFunc y negate) (Note 12))) ((applyFunc y reverse) (sortBy (compareNoteEv) x)))
  applyFunc y f = if y < 0 then f else id
  inv _ [] = []
  inv [] x = x
  inv (y : ys) ((Event c t a v) : xs) = inv ys ((applyFunc y reverse) (sortBy compareNoteEv (Event c t a (insert "note" (add y v) v) : xs)))
  add y x = VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" x) + y

inverse :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap
inverse pt = patternify invWith (segment 1 $ pt)

openWith :: Bool -> Pattern ValueMap -> Pattern ValueMap
openWith y = withEvents aux
 where
  aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
  steppityIn x =
    mapMaybe (\(n, ev) -> return ev) $
      enumerate (sortBy (compareNoteEv) $ if (y) then (open x) else x)
  open (xs : []) = [xs]
  open (xs : ys : []) = [xs, ys]
  open ((Event c1 t1 a1 v1) : ys : (Event c2 t2 a2 v2) : x) =
    (Event c1 t1 a1 (sub v1)) : (Event c2 t2 a2 (sub v2)) : ys : x
  sub m = insert "note" (VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" m) - 12) m

open :: Pattern Bool -> Pattern ValueMap -> Pattern ValueMap
open pt = patternify openWith (segment 1 $ pt)

dropWith :: String -> Pattern ValueMap -> Pattern ValueMap
dropWith y = withEvents aux
 where
  aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) es)
  steppityIn x = mapMaybe (\(n, ev) -> return ev) $ enumerate (drop y (reverse x))
  drop "0" (xs) = reverse $ xs
  drop "2" (xs : (Event c t a v) : x) = reverse $ xs : x ++ [(Event c t a (sub v))]
  drop "3" (xs : ys : (Event c t a v) : x) = reverse $ xs : ys : x ++ [(Event c t a (sub v))]
  drop "2p3" (xs : (Event c1 t1 a1 v1) : (Event c2 t2 a2 v2) : x) = reverse $ xs : x ++ (Event c1 t1 a1 (sub v1)) : (Event c2 t2 a2 (sub v2)) : []
  drop "4" (ws : xs : ys : (Event c t a v) : x) = reverse $ ws : xs : ys : x ++ [(Event c t a (sub v))]
  drop "2p4" (ws : (Event c1 t1 a1 v1) : ys : (Event c2 t2 a2 v2) : x) = reverse $ ws : ys : x ++ (Event c1 t1 a1 (sub v1)) : (Event c2 t2 a2 (sub v2)) : []
  drop _ x = reverse x
  sub m = insert "note" (VN $ (fromMaybe (Note 0) $ getN' $ findWithDefault (VN 0) "note" m) - 12) m

drop :: Pattern String -> Pattern ValueMap -> Pattern ValueMap
drop pt = patternify dropWith (segment 1 $ pt)

updown = fastspread ($) [id, rev]

updownBy t p = fast t $ spread ($) [id, rotR 1] $ spread ($) [id, rev] $ slow t $ p

updown' p = updownBy 2 p

downup = fastspread ($) [rev, id]

downupBy t p = fast t $ spread ($) [rotR 1, id] $ spread ($) [id, rev] $ slow t $ p

downup' p = downupBy 2 p

toTuple :: [String] -> (Pattern Int, Pattern String)
toTuple [a, b] = (parseBP_E a, parseBP_E b)

filter' :: (Bool -> Bool) -> String -> String
filter' f st = filter (\x -> f $ isDigit x) st

getPt :: Integer -> [(Integer, Pattern a)] -> Pattern a
getPt label patterns = case Prelude.lookup label patterns of
  Just pattern -> pattern
  Nothing -> error "Pattern not found"

ur' :: Pattern Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a
ur' t outer_p ps fs = slow t $ unwrap $ adjust <$> timedValues (getPat . split <$> outer_p)
 where
  split = wordsBy (== ':')
  getPat (s : xs) = (match s, transform xs)
  -- TODO - check this really can't happen..
  getPat _ = error "can't happen?"
  match s = fromMaybe silence $ Prelude.lookup s ps'
  ps' = map (fmap (fast t)) ps
  adjust (a, (p, f)) = f a p
  transform (x : _) a = transform' x a
  transform _ _ = id
  transform' str (Arc s e) p = s `rotR` inside (pure $ 1 / (e - s)) (matchF str) p
  matchF str = fromMaybe id $ Prelude.lookup str fs
  timedValues = withEvent (\(Event c (Just a) a' v) -> Event c (Just a) a' (a, v)) . filterDigital

gm = pF "gm"

mode' name pt = 0.01 <~ s name # note pt

mode'' name modePt pt = stack [pt, 0.005 <~ s name <| note modePt]

-- Generic version for streams
streamUnset :: Stream -> String -> IO ()
streamUnset stream k = modifyMVar_ (sStateMV stream) (return . Map.delete k)

presetFile = pS "presetFile"

songNamePt = pS "songName"

cP' d s = innerJoin $ parseBP_E <$> _cX d getS s

enableLiveGit :: Bool -> String -> Map.Map String (Pattern ValueMap) -> Map.Map String (Pattern ValueMap)
enableLiveGit (False) _ st = st
enableLiveGit (True) git st = Map.insert git (silence) $ Map.insert "loop" (s "liveGit") st

-- Transform Map1 to Map2 with "silence" as the default value for missing keys
transposeDrumMap :: (Ord a) => Map String (Map String (Pattern a)) -> Map String [(String, Pattern a)]
transposeDrumMap map1 =
  Map.fromList [(innerKey, constructInnerList innerKey) | innerKey <- allInnerKeys]
 where
  -- Collect all possible inner keys across all outer maps
  allInnerKeys = Map.keys $ Map.unions (Map.elems map1)
  -- Build a list for a given inner key
  constructInnerList innerKey =
    [ (outerKey, fromMaybe silence (Map.lookup innerKey innerMap))
    | (outerKey, innerMap) <- Map.toList map1
    ]

inhabit' :: (Ord k) => Pattern String -> (Pattern a -> b) -> Map k [(String, Pattern a)] -> [(k, b)]
inhabit' pt cp drumMap = foldl (\acc x -> (x, cp $ inhabit (Map.findWithDefault ([("1", silence)]) x drumMap) (pt)) : acc) [] (Map.keys drumMap)

transformProgs c a b = inhabit (zip (map (\x -> show x) [1 ..]) (map (\x -> prog x b) c)) a

-- transformDrums xs pt cp = inhabit' pt cp $ transposeDrumMap ( (Map.fromList (zip (map (\x -> show x)[1..]) (map (\x -> Map.fromList x) xs))))
transformDrums xs drumBankNr pt cp = inhabit' pt cp $ transposeDrumMap ((Map.fromList (zip (map (\x -> show x) [1 ..]) (map (\x -> Map.fromList (map (\y -> (fst y, snd y # drumBank drumBankNr)) x)) xs))))

mkProgs :: IO (IORef (Map String (Sheet ValueMap)), String -> Sheet ValueMap -> IO (Pattern String -> Pattern Int -> Pattern ValueMap))
mkProgs = do
  ref <- newIORef (Map.fromList [])
  let transformProgs refs s pt = ((inhabit (Prelude.map (\x -> (fst x, prog (snd x) pt)) refs)) s)
  let combine = transformProgs . Map.toList <$> readIORef ref
  let add x ds = do
        modifyIORef ref (Map.insert x ds)
        combine
  return (ref, add)

mkDrums :: Pattern Int -> IO (IORef (Map String [(String, Pattern ValueMap)]), String -> [(String, Pattern ValueMap)] -> IO (Pattern String -> (Pattern ValueMap -> Pattern ValueMap) -> [(String, Pattern ValueMap)]))
mkDrums mainDrumbank = do
  ref <- newIORef Map.empty
  let transformDrums refs pt cp = inhabit' pt cp (transposeDrumMap (Map.fromList (Prelude.map (\x -> (fst x, Map.fromList $ Prelude.map (\y -> (fst y, (|< drumBank mainDrumbank) $ snd y)) (snd x))) (Map.toList refs))))
  let combine = transformDrums <$> (readIORef ref)
  let add x ds = do
        modifyIORef ref (Map.insert x ds) -- more efficient
        combine -- absolutely not efficient
  return (ref, add)

(<:) :: b -> Pattern a -> (b, Pattern a)
(<:) = (,)

infixr 0 <:

mergeStreams :: [(String, ControlPattern)] -> [(String, ControlPattern)] -> [(String, ControlPattern)]
mergeStreams a b = Map.toList $ Map.union (Map.fromList b) (Map.fromList a)
