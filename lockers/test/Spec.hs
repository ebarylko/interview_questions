import Test.Hspec

import qualified Data.Map as Map

import Data.Maybe (mapMaybe, catMaybes)

import Data.List (singleton)

import Data.Function ((&))

import Lib (createNLockers, Lockers(..), LockerSize(..),  toPositive, idToLocker, lockerSizeToLockers, toLockerIds, toLocker)

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
                                 Map.fromListWith (++)
                                 (zip (cycle [Tiny .. ExtraLarge]) (map singleton expectedLockers))
                               }


        fmap createNLockers (toPositive 6) `shouldBe` Just expected
