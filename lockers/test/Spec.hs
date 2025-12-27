import Test.Hspec

import qualified Data.Map as Map

import Lib (createNLockers, Lockers, Locker, LockerId, LockerSize, Positive, toPositive, unsafeToLocker, unsafeToLockerId, )

main :: IO ()


main = hspec $ do
  context "Lockers" $ do
    describe "When creating six lockers" $ do
      it "Two extra small lockers and one locker of every other size will ge generated" $ do
        let num_of_lockers = toPositive 6
        let actual = CreateNLockers num_of_lockers
        let expected = Lockers{idToLocker= Map.fromList $ zip (map unsafeToLockerId [1..6]) [unsafeToLocker 1 Tiny, unsafeToLocker 2 Small, unsafeToLocker 3 Medium, unsafeToLocker 4 Large, unsafeToLocker 5 ExtraLarge, unsafeToLocker 6 Tiny],
                               lockerSizeToLockers= Map.fromList [(Tiny, [unsafeToLocker 1 Tiny, unsafeToLocker 6 Tiny]),
                                                                 (Small, [unsafeToLocker 2 Small])]

                                 }



    
-- main = putStrLn "Test suite not yet implemented"
