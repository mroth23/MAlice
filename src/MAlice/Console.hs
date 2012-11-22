module Main where

import System.Environment
import System.IO
import qualified MAlice.Parsing.Parser as MP

main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  putStrLn $ MP.parseMAlice code $ args !! 0