module Sound.Tidal.MIDISpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Ratio ((%))

import Sound.Tidal.MIDI

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "transformDur" $ do
    -- 4/4 time signatures
    it "should transform empty time signatures correctly" $ do
      let ts = []
          pt = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
               ,("noteOn",0 % 1,0.0,1 % 1),("noteOff",1 % 1,0.0,2 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)
          ,("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)]
    it "should transform 4/4 time signatures correctly" $ do
      let ts = [(1%1, 4, 2)]
          pt = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
               ,("noteOn",0 % 1,0.0,1 % 1),("noteOff",1 % 1,0.0,2 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)
          ,("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)]
    -- Different time signatures
    it "should transform two different time signatures without rests correctly" $ do
      let ts = [(1 % 1, 4, 2), (2%1, 5, 2)]
          pt = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
               ,("noteOn",0 % 1,0.0,1 % 1),("noteOff",1 % 1,0.0,2 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)
          ,("noteOn",0 % 1,0.0),("noteOff",5 % 4,0.0)]
    it "should transform two different time signatures with a single rest correctly" $ do
      let ts = [(1 % 1, 4, 2), (2%1, 7, 3), (3%1, 5, 2)]
          pt = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
               ,("noteOn",1 % 1,0.0,2 % 1),("noteOff",1 % 1,0.0,3 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)
          ,("noteOn",7 % 8,0.0),("noteOff",5 % 4,0.0)]
    it "should transform two different time signatures with a two rests of different time signatures correctly" $ do
      let ts = [(1 % 1, 4, 2), (2%1, 3, 3), (3%1, 7, 2), (4%1, 5, 2)]
          pt = [("noteOn",0 % 1,0.0,0 % 1),("noteOff",1 % 1,0.0,1 % 1)
               ,("noteOn",2 % 1,0.0,3 % 1),("noteOff",1 % 1,0.0,4 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",0 % 1,0.0),("noteOff",4 % 4,0.0)
          ,("noteOn",17 % 8,0.0),("noteOff",5 % 4,0.0)]
    -- Start pattern with a rest
    it "should transform with starting rests and different time signatures correctly" $ do
      let ts = [(1 % 1, 4, 2), (2 % 1, 2, 2), (3 % 1, 7, 3)]
          pt = [("noteOn",2 % 1,0.0,2 % 1),("noteOff",1 % 1,0.0,3 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",6 % 4,0.0),("noteOff",7 % 8,0.0)]
    it "should transform 4/4 time signatures with starting rest correctly" $ do
      let ts = []
          pt = [("noteOn",1 % 2,0.0,1 % 2),("noteOff",1 % 2,0.0,1 % 1)
               ,("noteOn",1 % 2,0.0,3 % 2),("noteOff",1 % 2,0.0,2 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",1 % 2,0.0),("noteOff",1 % 2,0.0)
          ,("noteOn",1 % 2,0.0),("noteOff",1 % 2,0.0)]
    it "should transform two different time signatures with starting rest correctly" $ do
      let ts = [(1 % 1, 4, 2) , (2 % 1, 7, 3)]
          pt = [("noteOn",1 % 2,0.0,1 % 2),("noteOff",1 % 2,0.0,1 % 1)
               ,("noteOn",2 % 7,0.0,9 % 7),("noteOff",5 % 7,0.0,2 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",1 % 2,0.0),("noteOff",1 % 2,0.0)
          ,("noteOn",2 % 8,0.0),("noteOff",5 % 8,0.0)]
    it "should transform two different time signatures with starting rest and a rest in between correctly" $ do
      let ts = [(1 % 1, 4, 2), (2 % 1, 2, 2), (3 % 1, 7, 3)]
          pt = [("noteOn",1 % 2,0.0,1 % 2),("noteOff",1 % 2,0.0,1 % 1)
              ,("noteOn",9 % 7,0.0,16 % 7),("noteOff",5 % 7,0.0,3 % 1)]
      transformDur ts pt `shouldBe`
          [("noteOn",1 % 2,0.0),("noteOff",1 % 2,0.0)
          ,("noteOn",6 % 8,0.0),("noteOff",5 % 8,0.0)]
