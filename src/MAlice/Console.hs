module Main where

import System.Environment
import System.IO
import qualified MAlice.Parsing.Parser as MP
import MAlice.CodeGeneration.CodeGen
import qualified MAlice.CodeGeneration.AssemblyCodeGenerator as ASM
main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  case (MP.mparse code $ args !! 0) of
    Left err -> putStrLn err
    Right result -> do
      putStrLn "Program parsed and type-checked successfully!"
      case runCodeGen result ASM.generateCode of
        Left err -> putStrLn err
        Right instrs -> putStrLn "Generated code successfully!"