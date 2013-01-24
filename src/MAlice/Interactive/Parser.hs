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

-- Calls the main parser to parse a whole file
parseFile :: String -> String -> Either String (Program, ParserState)
parseFile code path = mparse code path

-- Parse some user input with the main parser
parseUserInput :: ParserState -> String ->Either String (UserInput, ParserState)
parseUserInput pst code =
  case runParser parseUserCommand pst "user input" code of
    Left err -> Left $ show err
    Right (ast, re) ->
      case errors . errorList $ re of
           [] -> Right (ast, re)
           _  -> Left . show $ errorList re

-- Parse a user command (any of CompoundStmt, Expr, Decl or a command)
parseUserCommand :: MParser (UserInput, ParserState)
parseUserCommand = do
  result <- choice [try evalStmt, try evalExpr, try command, evalDecl]
  finalState <- getState
  return (result, finalState)

-- Parses a compound statement, so you can write something like
-- x became 5 then y became 3 then ackermann(x,y) said Alice.
-- (Don't try this particular example because it will recurse for ages)
evalStmt :: MParser UserInput
evalStmt = do
  st <- compoundStmt
  eof
  return $ EvalStmt st

-- Parses an expression, like ~(5%3)/12 or x * y % z
evalExpr :: MParser UserInput
evalExpr = do
  ex <- expr
  eof
  return $ EvalExpr ex

-- Parses a declaration of any variable or function (if you can trick the shell
-- into inputting a whole function body)
evalDecl :: MParser UserInput
evalDecl = do
  d <- decl
  eof
  return $ EvalDecl d

-- Parses a command like :q, :?, or :r etc.
command :: MParser UserInput
command = try $ (do
  _ <- char ':'
  l <- oneOf "lrq?"
  case l of
    '?' -> eof >> (return $ Command Help)
    'q' -> eof >> (return $ Command Quit)
    'r' -> eof >> (return $ Command ReloadFile)
    'l' -> space >> manyTill anyChar eof >>= return . Command . LoadFile)