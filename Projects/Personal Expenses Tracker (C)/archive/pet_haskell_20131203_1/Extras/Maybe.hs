module Extras.Maybe where

lastJust :: [Maybe a] -> Maybe a
lastJust = foldl (\a v -> case v of Nothing -> a; _ -> v) Nothing
