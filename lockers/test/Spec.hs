import Test.Hspec

import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Data.Set (singleton, union)

import Data.Function ((&))

import Lib (createNLockers, Lockers(..), LockerSize(..),  size, toPositive, idToLocker, lockerSizeToLockers, toLockerIds, toLocker)

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two tiny lockers and one locker of every other size will be generated" $ do
        let expectedLockers = zipWith toLocker [1..6] (cycle [Tiny .. ExtraLarge]) & catMaybes
        let expected = Lockers { idToLocker =
                                 Map.fromList 
                                 (zip
                                  (toLockerIds [1 .. 6])
                                  expectedLockers),
                                 lockerSizeToLockers =
                                 Map.fromListWith union $
                                 map (\locker -> (size locker, singleton locker)) expectedLockers
                               }


        fmap createNLockers (toPositive 6) `shouldBe` Just expected
