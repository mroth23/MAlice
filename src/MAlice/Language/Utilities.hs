module MAlice.Language.Utilities
       ( showTypes
       ) where

import MAlice.Language.Types

-- |Pretty prints a list of types
showTypes :: Show a => [a] -> String
showTypes [] = ""
showTypes ts =
  "(" ++ concatMap ((++ ", ") . show) (init ts) ++
  (show . last $ ts) ++ ")"