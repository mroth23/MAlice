module MAlice.Interactive.Shell where

import MAlice.Interactive.Parser
import MAlice.Interactive.Types
import MAlice.Parser.Parser
import MAlice.Parser.ParserState

import System.Environment
import System.IO


main :: IO ()
main = do
  userInputLoop initState (RuntimeState [])

userInputLoop :: ParserState -> RuntimeState -> IO ()
userInputLoop pst rts = do
  putStr ">"
  input <- getLine
  case parseUserInput pst input of
    Left err -> putStrLn err >> userInputLoop pst rts
    Right (i, pst') -> (putStrLn . show $ i) >> userInputLoop pst' rts