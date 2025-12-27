module Lib
    ( someFunc, createNLockers
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

newtype Positive = Positive { unPositive :: Int } 

toPositive :: Int -> Maybe Positive
toPositive n = if (n <= 0) then Nothing else Just (Positive n)

-- We can't export unPositive, because unPositive can be used
-- to update the field.  Trivially renaming it to getPositive
-- ensures that getPositive can only be used to access the field
getPositive :: Positive -> Int
getPositive = unPositive


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type LockerId = Positive

data LockerSize = Tiny | Small | Medium | Large | ExtraLarge  

data Locker = Locker {
  id:: LockerId,
  size:: LockerSize}

data Lockers = Lockers {
  idToLocker:: Map LockerId Locker,
  lockerSizeToLockers:: Map LockerSize [Locker],
  isValidLockerId:: LockerId -> Bool} 

{- Takes a positive number n and creates n lockers with the sizes of the
lockers being either tiny, small, medium, large, or extra large
-}
createNLockers:: Positive -> Lockers

createNLockers numOfLockers = Lockers Map.empty Map.empty (const True)


