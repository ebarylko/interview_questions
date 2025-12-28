import Test.Hspec

import qualified Data.Map as Map

import Lib (createNLockers, Lockers(..), LockerSize(..),  toPositive, unsafeToLocker, unsafeToLockerId, idToLocker, lockerSizeToLockers)

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two extra small lockers and one locker of every other size will ge generated" $ do
        let expected = Lockers { idToLocker =
                                 Map.fromList 
                                 (zip
                                  (map unsafeToLockerId [1 .. 6])
                                  [ unsafeToLocker 1 Tiny,
                                    unsafeToLocker 2 Small,
                                    unsafeToLocker 3 Medium,
                                    unsafeToLocker 4 Large,
                                    unsafeToLocker 5 ExtraLarge,
                                    unsafeToLocker 6 Tiny]),
                                 lockerSizeToLockers =
                                 Map.fromList
                                 [ (Tiny, [unsafeToLocker 1 Tiny, unsafeToLocker 6 Tiny])
                                 , (Small, [unsafeToLocker 2 Small])
                                 , (Medium, [unsafeToLocker 3 Medium])
                                 , (Large, [unsafeToLocker 4 Large])
                                 , (ExtraLarge, [unsafeToLocker 5 ExtraLarge])
                                 ]
                               }


        (fmap createNLockers (toPositive 6)) `shouldBe` Just expected
