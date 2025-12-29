module Lib
    ( createNLockers, LockerId, Lockers(..), Locker, LockerSize(..), toPositive, toLockerIds, toLocker
    ) where

import Data.Maybe (fromJust, mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Positive = Positive { unPositive :: Int } deriving (Eq, Ord, Show)

toPositive :: Int -> Maybe Positive
toPositive n = if (n <= 0) then Nothing else Just (Positive n)

-- We can't export unPositive, because unPositive can be used
-- to update the field.  Trivially renaming it to getPositive
-- ensures that getPositive can only be used to access the field
getPositive :: Positive -> Int
getPositive = unPositive


type LockerId = Positive

data LockerSize = Tiny | Small | Medium | Large | ExtraLarge  deriving (Eq, Ord, Show, Enum)

data Locker = Locker {
  uid:: LockerId,
  size:: LockerSize} deriving (Eq, Show)


{- Takes a number that potentially represents a valid id, a locker size, and
attempts to construct a locker from the given arguments. Returns None if the number does
not represent a valid id.
-}
  
toLocker :: Int -> LockerSize -> Maybe Locker

toLocker potentialId sze =  Locker <$> toPositive potentialId <*> pure sze


  {- Takes a collection of numbers that may represent a valid id for a locker and
returns only the numbers that are valid ids
-}
toLockerIds :: [Int] -> [LockerId]

toLockerIds =  mapMaybe toPositive


  {- This data type represents the remaining lockers available in a warehouse,
where the lockers come in five different sizes. 
-}
data Lockers = Lockers {
  idToLocker:: Map LockerId Locker,
  lockerSizeToLockers:: Map LockerSize [Locker]}  deriving (Eq, Show)

{- Takes a positive number n and creates n lockers with the sizes of the
lockers being either tiny, small, medium, large, or extra large
-}
createNLockers:: Positive -> Lockers

createNLockers numOfLockers = Lockers Map.empty Map.empty 


