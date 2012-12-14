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
    Right (ast, re) ->
      case errors . errorList $ re of
           [] -> Right (ast, re)
           _  -> Left . show $ errorList re

parseUserCommand :: MParser (UserInput, ParserState)
parseUserCommand = do
  result <- choice [try evalStmt, try evalExpr, try command, evalDecl]
  finalState <- getState
  return (result, finalState)

evalStmt :: MParser UserInput
evalStmt = do
  st <- compoundStmt
  eof
  return $ EvalStmt st

evalExpr :: MParser UserInput
evalExpr = do
  ex <- expr
  eof
  return $ EvalExpr ex

evalDecl :: MParser UserInput
evalDecl = do
  d <- decl
  eof
  return $ EvalDecl d

command :: MParser UserInput
command = try $ (do
  _ <- char ':'
  l <- oneOf "lrq?"
  case l of
    '?' -> eof >> (return $ Command Help)
    'q' -> eof >> (return $ Command Quit)
    'r' -> eof >> (return $ Command ReloadFile)
    'l' -> space >> manyTill anyChar eof >>= return . Command . LoadFile)