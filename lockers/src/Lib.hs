module Lib
    ( createNLockers, LockerId, Lockers(..), Locker, LockerSize(..), toPositive, toLockerIds, toLocker, size, requestLocker
    ) where

import qualified Data.Set as Set 
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.List (groupBy, sortOn)
import qualified Data.Map as Map
import Control.Arrow ((&&&))


newtype Positive = Positive { unPositive :: Int } deriving (Eq, Ord, Show)

toPositive :: Int -> Maybe Positive
toPositive n = if n <= 0 then Nothing else Just (Positive n)

-- We can't export unPositive, because unPositive can be used
-- to update the field.  Trivially renaming it to getPositive
-- ensures that getPositive can only be used to access the field
getPositive :: Positive -> Int
getPositive = unPositive


type LockerId = Positive

data LockerSize = Tiny | Small | Medium | Large | ExtraLarge  deriving (Eq, Ord, Show, Enum)

data Locker = Locker {
  uid:: LockerId,
  size:: LockerSize} deriving (Eq, Show, Ord)


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
keeping track of the availability of each locker and how many lockers of each size
are unused. The lockers come in five different sizes (see definition of LockerSize above). 
-}
data Lockers = Lockers {
  idToLocker:: Map LockerId Locker,
  lockerSizeToLockers:: Map LockerSize (Set.Set Locker)}  deriving (Eq, Show)

{- Takes a positive number n and creates n lockers with the sizes of the
lockers being either tiny, small, medium, large, or extra large
-}
createNLockers:: Positive -> Lockers

lockersToAvailabilityTracker :: [Locker] -> Map LockerId Locker

lockersToAvailabilityTracker locs = Map.fromList $ map (uid &&& id) locs


lockersToSizeTracker :: [Locker] -> Map LockerSize (Set.Set Locker)

isSameSize :: Locker -> Locker -> Bool

isSameSize = (==) `on` size

lockersToSizeTracker = Map.fromList . zip [Tiny .. ExtraLarge]  . map Set.fromList . groupBy isSameSize  . sortOn size


createNLockers numOfLockers = uncurry Lockers .
  (lockersToAvailabilityTracker &&& lockersToSizeTracker)
  . mapMaybe (uncurry toLocker) $
  zip [1 .. getPositive numOfLockers] (cycle allSizes)
  where
    allSizes = [Tiny .. ExtraLarge]

{-
Takes the size of a package, information about available lockers, and returns the id of the first locker that
is of the same size as the package. Returns none if no such lockers are available.
-}
requestLocker :: LockerSize -> Lockers -> Maybe LockerId

requestLocker size lockerInfo = Just $ (head . toLockerIds) [9]
 


data LockerAccessError = InvalidAccess LockerId | InUse | PackageDoesNotFit

type LockersUpdate a = (Lockers, a)

addPackage :: LockerSize -> Lockers -> Either LockerAccessError (LockersUpdate LockerId)

data LockerRemovalError = InvalidRemoval LockerId | NotInUse

removePackage :: LockerId -> Lockers -> Either LockerRemovalError (LockersUpdate ())


