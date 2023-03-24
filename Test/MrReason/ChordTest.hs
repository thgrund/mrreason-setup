{-# LANGUAGE OverloadedStrings #-}

module Test.MrReason.ChordTest where

import MrReason
import Test.TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>), drop)

import Sound.Tidal.Params
import Sound.Tidal.Pattern

test :: Microspec ()
test = 
   describe "Chord function tests" $ do
     describe "inv" $ do
        it "should keep the original pattern with value 0" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = inverse 0 $ note "[0,1,2,3]"
             expectedResult = note "[0,1,2,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should add 12 to first element and sort" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = inverse 1 $ note "[0,1,2,3]"
             expectedResult = note "[1,2,3,12]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should add 12 five time to the lowest value each round" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = inverse 5 $ note "[0,2,3,1]"
             expectedResult = note "[13,14,15,24]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should subtract 12 to last element and sort" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = inverse (-1) $ note "[0,1,2,3]"
             expectedResult = note "[-9,0,1,2]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should subtract 12 five time to the highest value each round" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = inverse (-5) $ note "[0,2,3,1]"
             expectedResult = note "[-21,-12,-11,-10]"
             in
               compareP overTimeSpan testMe expectedResult
     describe "open" $ do
        it "should keep the original pattern with value f" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = open "f" $ note "[0,1,2,3]"
             expectedResult = note "[0,1,2,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value t" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = open "t" $ note "[0,1,2,3]"
             expectedResult = note "[-12,-10,1,3]"
             in
               compareP overTimeSpan testMe expectedResult
     describe "drop" $ do
        it "should keep the original pattern with value 0" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "0" $ note "[0,1,2,3]"
             expectedResult = note "[0,1,2,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value 2" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "2" $ note "[0,1,2,3]"
             expectedResult = note "[-10,0,1,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value 3" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "3" $ note "[0,1,2,3]"
             expectedResult = note "[-11,0,2,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value 4" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "4" $ note "[0,1,2,3]"
             expectedResult = note "[-12,1,2,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value 2p3" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "2p3" $ note "[0,1,2,3]"
             expectedResult = note "[-11,-10,0,3]"
             in
               compareP overTimeSpan testMe expectedResult
        it "should apply open to the pattern with value 2p4" $ do
           let
             overTimeSpan = (Arc 0 1)
             testMe = drop "2p4" $ note "[0,1,2,3]"
             expectedResult = note "[-12,-10,1,3]"
             in
               compareP overTimeSpan testMe expectedResult
