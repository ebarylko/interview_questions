module Lib
    ( mkLockers, LockerId, Lockers(..), Locker, LockerSize(..), toPositive, toLockerIds, toLocker, size,  removePackage, LockerRemovalError(..), requestLocker
    ) where

import qualified Data.Set as Set 
import Data.Set (findMin, delete, union)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import Data.Map (Map, lookup)
import Data.List (groupBy, sortOn, null)
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
  occupied:: Map LockerId Locker,
  available:: Map LockerSize (Set.Set Locker),
  largestId:: LockerId}  deriving (Eq, Show)

{- Takes a positive number n and creates n lockers with the sizes of the
lockers being either tiny, small, medium, large, or extra large
-}
mkLockers:: Positive -> Lockers

getPositiveInt = unPositive

mkLockers numOfLockers = Lockers noneOccupied availableLockers numOfLockers
  where
    highestLockerId = getPositiveInt numOfLockers
    allSizes = [Tiny .. ExtraLarge]
    allIds = mapMaybe toPositive [1 .. highestLockerId]
    noneOccupied = Map.empty
    availableLockers = groupBySize $ zipWith Locker allIds (cycle allSizes)
    groupBySize = foldr insertLocker Map.empty 
    insertLocker locker = Map.insertWith Set.union (size locker) (Set.singleton locker) 

type LockersUpdate a = (Lockers, a)


-- Takes the size of a package, information about available lockers, and returns the id of the first locker that
-- is of the same size as the package. Returns none if no such lockers are available.

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a

filterMaybe pred m = m >>= (\v -> if pred v then Just v else Nothing)

toLockerId maybeId = (head. toLockerIds) [maybeId]

emptyLockers = Lockers Map.empty Map.empty

{-
Takes a locker, l, information about which lockers are in use, and
returns the updated collection of occupied lockers, now containing l
-}
updateOccupiedLockers :: Locker -> Lockers -> Map LockerId Locker

-- updateOccupiedLockers loc locs = locs & occupied & Map.insert (uid loc) loc

updateOccupiedLockers loc = Map.insert (uid loc) loc . occupied  

{-
Takes a locker, l, information about which lockers are available, and
returns the new set of available lockers with the exclusion of l
-}
updateAvailableLockers :: Locker -> Lockers -> Map LockerSize (Set.Set Locker)

updateAvailableLockers loc = Map.update (Just . delete loc) (size loc) . available


-- {-
-- Takes a locker that is not in use, l, information about which lockers are available/unavailable,
-- and updates the information to note that l is in use and cannot be used for subsequent packages.
-- -}
updateLockers :: Locker -> Lockers -> Lockers

updateLockers loc locs = Lockers (updateOccupiedLockers loc locs) (updateAvailableLockers loc locs) (largestId locs)

requestLocker :: LockerSize -> Lockers -> Maybe (LockersUpdate LockerId)

requestLocker size lockerInfo = available lockerInfo & Map.lookup size & filterMaybe (not . null) & fmap
  updateLocs
  where
    updateLocs = (flip updateLockers lockerInfo &&& uid) . findMin

data LockerRemovalError = InvalidRemoval LockerId | NotInUse deriving (Show, Eq)


maybeToEither :: l -> Maybe r  -> Either l r

maybeToEither err dat = case dat of
  Nothing -> Left err
  Just x -> Right x

mapRight :: (b -> c) -> Either a b -> Either a c

mapRight _ (Left l) = Left l
mapRight f (Right r) = Right (f r)

lookupLocker :: LockerId -> Lockers -> Either LockerRemovalError Locker

lookupLocker lId = maybeToEither NotInUse . Map.lookup lId . occupied 

removePackage :: LockerId -> Lockers -> Either LockerRemovalError (LockersUpdate ())

{-
Takes a locker, l, information about the available lockers and those in use, and
updates the information to note that l is no longer in use and is available
-}
updateOccupiedAndAvailable :: Locker -> Lockers -> LockersUpdate ()

updateOccupiedAndAvailable loc locs = 
  (Lockers updatedOccupied updatedAvailable (largestId locs),
   ())
  where
    updatedOccupied = Map.delete (uid loc) (occupied locs) 
    updatedAvailable = Map.adjust (Set.singleton loc & Set.union) (size loc) (available locs)

eitherFromPredicate :: (a -> Bool) -> (a -> error) -> a -> Either error a
eitherFromPredicate p mkErr x
  | p x       = Right x        -- If the predicate is satisfied, return Right
  | otherwise = Left $ mkErr x -- If the predicate is not satisfied, call the function to create the left value



removePackage lId locs =
  ensureIsInTheLockerGroup lId locs
  >>= flip ensureIsOccupied locs
  & mapRight (`updateOccupiedAndAvailable` locs)

  where

    ensureIsOccupied :: LockerId -> Lockers -> Either LockerRemovalError Locker
    ensureIsOccupied lId lockers = maybeToEither NotInUse $ Map.lookup lId (occupied lockers)

    ensureIsInTheLockerGroup :: LockerId  -> Lockers -> Either  LockerRemovalError LockerId
    ensureIsInTheLockerGroup lId lockers = eitherFromPredicate (<= largestId lockers) InvalidRemoval lId
