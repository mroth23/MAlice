module MAlice.Language.Utilities
       ( showMaybe
       , showTypes
       , maybeCheck
       , maybeCheck2
       ) where

import Data.Maybe
import MAlice.Language.Types

-- |Pretty prints maybe types for error output
showMaybe :: Show a => Maybe a -> String
showMaybe (Nothing) = "void"
showMaybe (Just a) = show a

-- |Pretty prints a list of maybe types
showTypes :: [Maybe Type] -> String
showTypes [] = ""
showTypes ts =
  "(" ++ concatMap ((++ ", ") . showMaybe) (init ts) ++
  (showMaybe . last $ ts) ++ ")"

-- |Helper function that flips the arguments of maybe around
maybeCheck :: b -> Maybe a -> (a -> b) -> b
maybeCheck = flip . maybe

-- |Similar to maybeCheck, but with two arguments
maybeCheck2 :: b -> Maybe a -> Maybe a -> (a -> a -> b) -> b
maybeCheck2 fail Nothing _ _ = fail
maybeCheck2 fail _ Nothing _ = fail
maybeCheck2 _ a a' f = f (fromJust a) (fromJust a')