{-# LANGUAGE OverloadedStrings #-}

module Test.MrReason.ProgressionTest where

import MrReason
import Test.TestUtils
import Test.Microspec

import Sound.Tidal.Params
import Sound.Tidal.Pattern
import Sound.Tidal.UI

sheet = Sheet {
    key = "c",
    mode = "major",
    numerals = "1",
    functions = id
}

test :: Microspec ()
test =
   describe "Function prog test" $ do
      describe "numerals pattern" $ do
        it "should create the correct pattern if you wrap the numerals with [ and ]" $ do
          let
            s1 = sheet {numerals = "1"}
            s2 = sheet {numerals = "[1]"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = prog s2 "[1,3,5,7]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you wrap the numerals with < and >" $ do
          let
            s1 = sheet {numerals = "1"}
            s2 = sheet {numerals = "<1>"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = prog s2 "[1,3,5,7]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you use the @ identifier" $ do
          let
            s1 = sheet {numerals = "[1@3 2]"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "1"
            expectedResult = segment 1 $ note "[0@3 2]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you use the ! identifier" $ do
          let
            s1 = sheet {numerals = "[1!3 2!]"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "1"
            expectedResult = segment 1 $ note "[0 0 0 2 2]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you use the * identifier" $ do
          let
            s1 = sheet {numerals = "[1*3 2]"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "1"
            expectedResult = segment 1 $ note "[[0 0 0] 2]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you use the / identifier" $ do
          let
            s1 = sheet {numerals = "[1/2]"}
            overTimeSpan = (Arc 0 1)
            testMe = fast 2 $ prog s1 "1"
            expectedResult = note "[0 0]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should create the correct pattern if you use the . identifier" $ do
          let
            s1 = sheet {numerals = "[1 1 . 2]"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "1"
            expectedResult = segment 1 $ note "[[0 0] 2]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet key" $ do
        it "should map the prog notes to numerals 1 of d major as d5" $ do
          let
            s1 = sheet {key = "d", numerals = "1"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[2,6,9,13]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numerals 1 of d6 major" $ do
          let
            s1 = sheet {key = "d6", numerals = "1"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[14,18,21,25]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numerals 1 of -2 (as) major" $ do
          let
            s1 = sheet {key = "-2", numerals = "1"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-2,2,5,9]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet numerals without identifier for c major" $ do
        it "should map the prog notes to numeral 1 of c major" $ do
          let
            overTimeSpan = (Arc 0 1)
            testMe = prog sheet "[1,3,5,7]"
            expectedResult = note "[0,4,7,11]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numeral 2 of c major" $ do
          let
            s1 = sheet {numerals = "2"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[2,5,9,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numeral -2 of c major" $ do
          let
            s1 = sheet {numerals = "-2"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-5,-1,2,5]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet numerals without identifier for c minor" $ do
        it "should map the prog notes to numeral 1 of c minor" $ do
          let
            s1 = sheet {mode = "minor"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[0,3,7,10]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numeral 2 of c minor" $ do
          let
            s1 = sheet {numerals = "2", mode = "minor"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[2,5,8,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should map the prog notes to numeral -2 of c minor" $ do
          let
            s1 = sheet {numerals = "-2", mode = "minor"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-5,-2,2,5]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet numerals with identifier for c major" $ do
        it "should apply 'o' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#o"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-12,-8,-5,-1]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'O' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#O"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[12,16,19,23]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 't' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#t"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-6,-2,1,5]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'T' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#T"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[6,10,13,17]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply '+' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#+"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[1,5,8,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply chained '+++' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#+++"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[3,7,10,14]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply '-' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#-"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-1,3,6,10]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply chained '---' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#---"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-3,1,4,8]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'd' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#d"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8,9]"
            expectedResult = note "[0,1,3,4,6,7,9,10,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'a' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#a"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7]"
            expectedResult = note "[0,3,4,7,8,11,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'S' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#S"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[7,11,14,17]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 's' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#s"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-5,-1,2,5]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply mixed identfiers '++So' identifier to numeral 1 of c major" $ do
          let
            s1 = sheet {numerals = "1#++So"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[-3,1,4,7]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet numerals with mode identifier for c major" $ do
        it "should apply 'I' identifier to numeral 1 of c major (override to Ionian)" $ do
          let
            s1 = sheet {numerals = "1#I"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,2,4,5,7,9,11,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'I' identifier to numeral 7 of c major (override to Ionian)" $ do
          let
            s1 = sheet {numerals = "7#I"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[11,12,14,16,17,19,21,23]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'D' identifier to numeral 1 of c major (override to Dorian)" $ do
          let
            s1 = sheet {numerals = "1#D"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,2,3,5,7,9,10,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'D' identifier to numeral 6 of c major (override to Dorian)" $ do
          let
            s1 = sheet {numerals = "6#D"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[9,10,12,14,15,17,19,21]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'P' identifier to numeral 1 of c major (override to Phrygian)" $ do
          let
            s1 = sheet {numerals = "1#P"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,1,3,5,7,8,10,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'P' identifier to numeral 5 of c major (override to Phrygian)" $ do
          let
            s1 = sheet {numerals = "5#P"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[7,8,10,12,13,15,17,19]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'L' identifier to numeral 1 of c major (override to Lydian)" $ do
          let
            s1 = sheet {numerals = "1#L"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,2,4,6,7,9,11,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'L' identifier to numeral 4 of c major (override to Lydian)" $ do
          let
            s1 = sheet {numerals = "4#L"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[6,7,9,11,12,14,16,18]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'M' identifier to numeral 1 of c major (override to Mixolydian)" $ do
          let
            s1 = sheet {numerals = "1#M"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,2,4,5,7,9,10,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'M' identifier to numeral 3 of c major (override to Mixolydian)" $ do
          let
            s1 = sheet {numerals = "3#M"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[4,5,7,9,10,12,14,16]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'A' identifier to numeral 1 of c major (override to Aeolian)" $ do
          let
            s1 = sheet {numerals = "1#A"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[0,2,3,5,7,8,10,12]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply 'A' identifier to numeral 2 of c major (override to Aeolian)" $ do
          let
            s1 = sheet {numerals = "2#A"}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,2,3,4,5,6,7,8]"
            expectedResult = note "[2,3,5,7,8,10,12,14]"
            in
              compareP overTimeSpan testMe expectedResult
      describe "sheet functions for c major" $ do
        it "should apply fast 2 for numeral 1 of c major" $ do
          let
            s1 = sheet {functions = (fast 2)}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1,3,5,7]"
            expectedResult = note "[0,4,7,11]!"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply rev for numeral 1 of c major" $ do
          let
            s1 = sheet {functions = (rev)}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1 3 5 7]"
            expectedResult = note "[11 7 4 0]"
            in
              compareP overTimeSpan testMe expectedResult
        it "should apply rev and fast 2 and ply in a chain for numeral 1 of c major" $ do
          let
            s1 = sheet {functions = (fast 2 . rev . (ply 2))}
            overTimeSpan = (Arc 0 1)
            testMe = prog s1 "[1 3 5 7]"
            expectedResult = note "[11! 7! 4! 0!]!"
            in
              compareP overTimeSpan testMe expectedResult
