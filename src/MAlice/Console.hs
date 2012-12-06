module Main where

import System.Environment
import System.IO
import qualified MAlice.IR.Types as IR
import qualified MAlice.IR.CodeGen as IR
import qualified MAlice.Parser.Parser as MP
import qualified MAlice.Parser.ParserState as MP
import qualified MAlice.Language.AST as AST

main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  let result = compile code (args !! 0)
  case result of
    Left err -> putStrLn err
    Right act -> act

compile :: String -> String -> Either String (IO ())
compile code name = do
  (parsedAST, _) <- parseCode code name
  irCode <- generateIR parsedAST
  return $ IR.showProgram irCode

parseCode :: String -> String -> Either String (AST.Program, MP.ParserState)
parseCode code name =
  MP.mparse code name

generateIR :: AST.Program -> Either String IR.Program
generateIR p =
  IR.generateIRCode p