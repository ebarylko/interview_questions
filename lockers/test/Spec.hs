import Test.Hspec

import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Data.Set (singleton, union)

import Data.Function ((&))

import Lib (createNLockers, Lockers(..), LockerSize(..),  size, toPositive, toLockerIds, toLocker, requestLocker)

lockers = toPositive

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two tiny lockers and one locker of every other size will be generated" $ do
        let expectedLockers = zipWith toLocker [1..6] [Tiny, Small, Medium, Large, ExtraLarge, Tiny] & catMaybes
        let expected = Lockers { occupied = Map.empty ,
                                 available =
                                 Map.fromListWith union $
                                 map (\locker -> (size locker, singleton locker)) expectedLockers
                               }


        fmap createNLockers (lockers 6) `shouldBe` Just expected

    describe "When requesting a locker in a size that is no longer available" $ do
      it "A locker id is not returned" $ do
        (lockers 3 >>= requestLocker Large . createNLockers) `shouldBe` Nothing

    describe "When requesting a locker in a size that is available" $ do
      it "A locker id corresponding to an unused locker is returned" $ do
        pending
