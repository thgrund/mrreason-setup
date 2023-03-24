{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec

import Test.MrReason.ChordTest
import Test.MrReason.ProgressionTest

main :: IO ()
main = microspec $ do 
  Test.MrReason.ChordTest.test
  Test.MrReason.ProgressionTest.test