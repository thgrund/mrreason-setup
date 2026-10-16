module Sound.MrReason.Numeral (Numeral (..), Modifier (..)) where

import Sound.Tidal.Boot
import Text.ParserCombinators.Parsec

data Modifier
  = Plus
  | Minus
  | TritonusSubstitutionUp
  | TritonusSubstitutionDown
  | HarmonicMode1
  | HarmonicMode2
  | HarmonicMode3
  | HarmonicMode4
  | HarmonicMode5
  | HarmonicMode6
  | HarmonicMode7
  | HarmonicMinor
  | OctaveUp
  | OctaveDown
  | SecondaryDominantUp
  | SecondaryDominantDown
  | Locrian
  | Diminished
  | Augmented
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  deriving (Eq, Read, Show)

data Numeral
  = Numeral {root :: Int, modifiers :: [Modifier]}
  -- Internal endpoint value; source positions live in its TPat atom.
  | NumeralPositionMarker [Modifier]
  deriving (Eq, Read, Show)

pModifier :: MyParser Modifier
pModifier =
      (char '+' >> return Plus)
  <|> (char 'p' >> return Plus)
  <|> (char '-' >> return Minus)
  <|> (char 'm' >> return Minus)
  <|> (char 'T'  >> return TritonusSubstitutionUp)
  <|> (char 't'  >> return TritonusSubstitutionDown)
  <|> (char 'O'  >> return OctaveUp)
  <|> (char 'o'  >> return OctaveDown)
  <|> (char 'S'  >> return SecondaryDominantUp)
  <|> (char 's'  >> return SecondaryDominantDown)
  <|> (char 'l'  >> return Locrian)
  <|> (char 'd'  >> return Diminished)
  <|> (char 'a'  >> return Augmented)
  <|> (char 'I'  >> return Ionian)
  <|> (char 'D'  >> return Dorian)
  <|> (char 'P'  >> return Phrygian)
  <|> (char 'L'  >> return Lydian)
  <|> (char 'M'  >> return Mixolydian)
  <|> (char 'A'  >> return Aeolian)
  <|> (try (string "H1") >> return HarmonicMode1)
  <|> (try (string "H2") >> return HarmonicMode2)
  <|> (try (string "H3") >> return HarmonicMode3)
  <|> (try (string "H4") >> return HarmonicMode4)
  <|> (try (string "H5") >> return HarmonicMode5)
  <|> (try (string "H6") >> return HarmonicMode6)
  <|> (try (string "H7") >> return HarmonicMode7)
  <|> (char 'H' >> return HarmonicMinor)

data ModifierList
  = ModifierList { getModifiers :: [Modifier] }
  | ModifierListPositionMarker [Modifier] -- Internal modifier-position accumulator.
  deriving (Eq, Read, Show)

pPositionedModifier :: MyParser (TPat ModifierList)
pPositionedModifier = do
  start <- getPosition
  modifier <- pModifier
  end <- getPosition
  let plusOffset = if modifier == Plus then 1 else 0
      position =
        ( (sourceColumn start - plusOffset, sourceLine start)
        , (sourceColumn end, sourceLine end)
        )
  -- socket.hs increments colStart; shift only Plus starts so they land on the glyph.
  return (TPat_Atom (Just position) (ModifierList [modifier]))

modifierPositionMarker :: ModifierList -> ModifierList
modifierPositionMarker (ModifierList modifiers) = ModifierListPositionMarker modifiers
modifierPositionMarker marker@(ModifierListPositionMarker _) = marker

-- EnumFromTo merges the modifier atoms' contexts without splitting their event.
combineModifierPatterns :: TPat ModifierList -> TPat ModifierList -> TPat ModifierList
combineModifierPatterns previous next =
  TPat_EnumFromTo (fmap modifierPositionMarker previous) next

pModifierList :: MyParser (TPat ModifierList)
pModifierList = do
  first <- pPositionedModifier
  rest <- many $
        (char ',' Prelude.*> pPositionedModifier)
    <|> pPositionedModifier
  return (foldl combineModifierPatterns first rest)

pNumeralModifiers :: MyParser (TPat ModifierList)
pNumeralModifiers = pModifierList

instance Enumerable ModifierList where
  fromTo (ModifierListPositionMarker previous) (ModifierListPositionMarker next) =
    pure (ModifierListPositionMarker (previous ++ next))
  fromTo (ModifierListPositionMarker previous) (ModifierList next) =
    pure (ModifierList (previous ++ next))
  fromTo a b = fastFromList [a, b]
  fromThenTo a b c = fastFromList [a, b, c]

instance Parseable ModifierList where
  tPatParser = pNumeralModifiers
  doEuclid = euclidOff

pRoot :: MyParser Int
pRoot = do
  s <- sign
  digits <- many1 digit
  return (fromIntegral (applySign s (read digits)))

pNumeral :: MyParser (TPat Numeral)
pNumeral = do
  rootStart <- getPosition
  r <- pRoot
  rootEnd <- getPosition
  let rootPos =
        ( (sourceColumn rootStart, sourceLine rootStart)
        , (sourceColumn rootEnd, sourceLine rootEnd)
        )
      rootMarker = TPat_Atom (Just rootPos) (NumeralPositionMarker [])
      toNumeral (ModifierList modifiers) = Numeral
        { root = r
        , modifiers = modifiers
        }
      toNumeral (ModifierListPositionMarker modifiers) = NumeralPositionMarker modifiers
  option
    (TPat_Atom (Just rootPos) (Numeral { root = r, modifiers = [] }))
    (do
      char ':'
      ms <- pPart pNumeralModifiers
      return $ TPat_EnumFromTo rootMarker (fmap toNumeral ms))


instance Parseable Numeral where
  tPatParser = pNumeral
  doEuclid = euclidOff

instance Enumerable Numeral where
  -- TPat_EnumFromTo combines source contexts while carrying modifier values.
  fromTo (NumeralPositionMarker previous) (NumeralPositionMarker next) =
    pure (NumeralPositionMarker (previous ++ next))
  fromTo (NumeralPositionMarker previous) (Numeral { root = rootValue, modifiers = next }) =
    pure (Numeral { root = rootValue, modifiers = previous ++ next })
  fromTo a b = fastFromList [a, b]
  fromThenTo a b c = fastFromList [a, b, c]



