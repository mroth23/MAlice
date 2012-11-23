module Main where

import System.Environment
import System.IO
import qualified MAlice.Parsing.Parser as MP

main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  case (MP.mparse code $ args !! 0) of
    Left err -> putStrLn err
    Right ast -> putStrLn . show $ ast