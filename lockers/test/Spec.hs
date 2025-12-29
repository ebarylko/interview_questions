import Test.Hspec

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Lib (createNLockers, Lockers(..), LockerSize(..),  toPositive, unsafeToLocker, unsafeToLockerId, idToLocker, lockerSizeToLockers, toLockerIds, toLocker)

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two tiny lockers and one locker of every other size will be generated" $ do
        let expected = Lockers { idToLocker =
                                 Map.fromList 
                                 (zip
                                  (toLockerIds [1 .. 6])
                                  (mapMaybe (uncurry toLocker) (zip [1..6]  (cycle [Tiny .. ExtraLarge])))),
                                 lockerSizeToLockers =
                                 Map.fromList
                                 [ (Tiny, [unsafeToLocker 1 Tiny, unsafeToLocker 6 Tiny])
                                 , (Small, [unsafeToLocker 2 Small])
                                 , (Medium, [unsafeToLocker 3 Medium])
                                 , (Large, [unsafeToLocker 4 Large])
                                 , (ExtraLarge, [unsafeToLocker 5 ExtraLarge])
                                 ]
                               }


        fmap createNLockers (toPositive 6) `shouldBe` Just expected
