module MAlice.Interactive.Parser
       ( parseFile
       , parseCommand
       ) where

import MAlice.Interactive.Types
import MAlice.Parser.Parser
import MAlice.Parser.ParserState
import MAlice.Language.AST

parseFile :: String -> String -> Either String (Program, ParserState)
parseFile code path = parseCode code path

parseUserCommand :: MParser UserInput
parseUserCommand = choice [evalStmt, evalExpr, command]

evalStmt :: MParser UserInput
evalStmt = do
  st <- compoundStmt
  return $ EvalStmt st

evalExpr :: MParser UserInput
evalExpr = do
  ex <- expr
  return $ EvalExpr ex

command :: MParser UserInput
command = try $ (do
  _ <- colon
  l <- oneOf "lrq"
  case l of
    'q' -> return $ Command Quit
    'r' -> return $ Command ReloadFile
    'l' -> do
      fileName <- many1 anyChar
      eof
      return $ Command (LoadFile fileName)
