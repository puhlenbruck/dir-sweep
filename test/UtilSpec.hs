module UtilSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Util

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "maybeDrop" $ do
    it "drops" $ do
      maybeDrop (Just 2) [1, 2, 3, 4, 5] `shouldBe` [3, 4, 5]
    it "empty list returns empty list" $ property $
      \n ->  maybeDrop n ([] :: [Int]) === []
    it "nothing leaves list unchanged" $ property $
      \as ->  maybeDrop Nothing (as :: [Int]) === as
    it "returns in equal or shorter list" $ property $
      \n as -> length (as :: [Int]) >= length (maybeDrop n as)

