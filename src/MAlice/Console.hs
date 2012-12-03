module Main where

import System.Environment
import System.IO
import qualified MAlice.Parser.Parser as MP
import MAlice.CodeGen.CodeGen
main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  case (MP.mparse code $ args !! 0) of
    Left err -> putStrLn err
    Right result -> do
      putStrLn "Program parsed and type-checked successfully!"