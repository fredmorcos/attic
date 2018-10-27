module Locker where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Eq, Show)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num lockers =
  case Map.lookup num lockers of
    Nothing            -> Left $ "Locker number " ++ show num ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show num ++ " already taken!"
