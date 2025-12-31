import Test.Hspec

import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Data.Set (singleton, union, empty)
import qualified Data.List as List
import Control.Applicative (liftA2)

import Data.Function ((&))

import Lib (createNLockers, Lockers(..), LockerSize(..),  size, toPositive, toLockerIds, toLocker,  removePackage, LockerRemovalError(..), requestLocker)

lockers = toPositive
toLockerId = toPositive
toLockerSize = toPositive

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two tiny lockers and one locker of every other size will be generated" $ do
        let expectedLockers = zipWith toLocker [1..6] [Tiny, Small, Medium, Large, ExtraLarge, Tiny] & catMaybes
        let toExpected maxId=  Lockers { occupied = Map.empty ,
                                         available =
                                           Map.fromListWith union $
                                           map (\locker -> (size locker, singleton locker)) expectedLockers,
                                         largestId = maxId}

        let expected =  toExpected <$> toLockerId 6


        fmap createNLockers (lockers 6) `shouldBe` expected

    describe "When requesting a locker in a size that is no longer available" $ do
      it "No locker id is returned" $ do
        (lockers 3 >>= requestLocker Large . createNLockers) `shouldBe` Nothing

    describe "When requesting a locker in a size that is available" $ do
      it "A locker id corresponding to an unused locker is returned and the updated locker store notes that this locker is in use" $ do
        let expectedLocker = toLocker 1 Tiny 
        let expectedId = toLockerId 1

        let expectedLockers = do
              lId <- expectedId
              loc <- expectedLocker
              sze <- toLockerSize 1
              pure $ Lockers (Map.singleton lId loc)
                            (Map.singleton Tiny empty) sze



        (lockers 1 >>= requestLocker Tiny . createNLockers) `shouldBe` liftA2 (,) expectedLockers expectedId

    describe "When removing a locker that does not exist" $ do
      it "An invalid locker id error is returned" $ do
        let invalidId = toLockerId 2

        removePackage <$> invalidId <*> (createNLockers <$> lockers 1) `shouldBe` fmap (Left . InvalidRemoval) invalidId

    describe "When removing a package from a valid locker that is not in use" $ do
      it "An not in use error is returned" $ do
        let validId = toLockerId 1

        removePackage <$> validId <*> (createNLockers <$> lockers 1) `shouldBe` ( Just . Left ) NotInUse 











