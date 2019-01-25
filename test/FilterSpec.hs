module FilterSpec (main, spec) where

import Data.Either
import Data.List (sortOn)
import Data.Maybe
import Data.Ord
import Data.Time.Clock

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances

import Sweep

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "filterFiles" $ do
    it "includes no files when no options given" $ property $
      \files -> filterFiles (filterOptions Nothing Nothing Nothing) files  === []
    it "include no files when only minKeep given" $ property $
      \files minKeep -> filterFiles (filterOptions Nothing Nothing minKeep) files === []
    it "includes n - maxKeep oldest files when maxKeep as only option" $ property $
      \files maxKeep -> filterFiles (filterOptions Nothing maxKeep Nothing) files === take (length files - fromMaybe (length files) maxKeep) (sortOn modifyTime files)
    it "includes files older than the threshold" $ property $
      \files thresholdTime -> filterFiles (filterOptions thresholdTime Nothing Nothing) files === filter (\file -> maybe False (modifyTime file <) thresholdTime) (sortOn modifyTime files)
    it "includes files older than the threshold after minKeep" $ property $
      \files thresholdTime minKeep -> filterFiles (filterOptions thresholdTime Nothing minKeep) files === filter (\file -> maybe False (modifyTime file <) thresholdTime) (reverse $ drop (fromMaybe 0 minKeep) (sortOn (Down . modifyTime) files))
  describe "validatedFilterOptions" $ do
    it "Gives Right when both Nothing" $
      validatedFilterOptions Nothing Nothing Nothing `shouldSatisfy` isRight
    it "Gives Right when max is Nothing" $ property $
      \minKeep -> validatedFilterOptions Nothing Nothing minKeep `shouldSatisfy` isRight
    it "Gives Right when min is Nothing" $ property $
      \maxKeep -> validatedFilterOptions Nothing maxKeep Nothing `shouldSatisfy` isRight
    it "Gives Right when max > min" $
      validatedFilterOptions Nothing (Just 3) (Just 2) `shouldSatisfy` isRight
    it "Gives Right when max = min" $
      validatedFilterOptions Nothing (Just 3) (Just 3) `shouldSatisfy` isRight
    it "Gives Left when max < min" $
      validatedFilterOptions Nothing (Just 1) (Just 2) `shouldSatisfy` isLeft

    
filterOptions :: Maybe UTCTime -> Maybe Int -> Maybe Int -> FilterOptions
filterOptions thresholdTime maxKeep minKeep = fromRight (error errorMessage)  (validatedFilterOptions thresholdTime maxKeep minKeep)
    where errorMessage = "Invalid filter options '" ++ show thresholdTime ++ ", " ++ show maxKeep ++ ", " ++ show minKeep ++ "'."

instance Arbitrary FileAndModTime where
  arbitrary = do
    time <- arbitrary
    name <- arbitrary
    return FileAndModTime{name=name, modifyTime=time}