module MAlice.Interactive.Parser
       ( parseFile
       , parseUserInput
       ) where

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import MAlice.Interactive.Types
import MAlice.Parser.Parser
import MAlice.Parser.ParserState
import MAlice.Language.AST

parseFile :: String -> String -> Either String (Program, ParserState)
parseFile code path = mparse code path

parseUserInput :: ParserState -> String ->Either String (UserInput, ParserState)
parseUserInput pst code =
  case runParser parseUserCommand pst "user input" code of
    Left err -> Left $ show err
    Right (ast, re) -> Right (ast, re)

parseUserCommand :: MParser (UserInput, ParserState)
parseUserCommand = do
  result <- choice [evalStmt, evalExpr, command]
  finalState <- getState
  return (result, finalState)

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
  _ <- char ':'
  l <- oneOf "lrq"
  case l of
    'q' -> return $ Command Quit
    'r' -> return $ Command ReloadFile
    'l' -> manyTill anyChar eof >>= \f -> return $ Command (LoadFile f))