module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt
import qualified MAlice.CodeGen.JavaByteCode    as J
import qualified MAlice.Language.AST            as AST
import qualified MAlice.Language.PrettyPrint    as PP
import qualified MAlice.Parser.Parser           as MP
import qualified MAlice.Parser.ParserState      as MP
import qualified MAlice.Transformation.Optimise as T
import qualified MAlice.Transformation.Rename   as T
import qualified MAlice.Transformation.Desugar  as T

import qualified MAlice.Interactive.Shell as I

main :: IO ()
main = do
  args <- getArgs
  let (actions, others, msgs) = getOpt RequireOrder options args
  putOthers others
  putMsgs msgs
  opts <- foldl (>>=) (return defaultOptions) actions
  let input  = optInput opts
      output = optOutput opts
      (i, f) = optInteractive opts
      pp     = optPPrint opts
      file   = inputFileName opts
  compileFile input output pp file


putOthers :: [String] -> IO ()
putOthers [] = return ()
putOthers os = error $ "unrecognised arguments: " ++ unwords os

putMsgs :: [String] -> IO ()
putMsgs [] = return ()
putMsgs ms = error $ concat ms ++ usageInfo header options

data Options = Options
               { optInteractive :: (Bool, Maybe String)
               , optInput :: IO String
               , inputFileName :: String
               , optPPrint :: String -> IO ()
               , optOutput :: String -> IO () }

defaultOptions :: Options
defaultOptions =
  Options { optInteractive = (False, Nothing)
          , optInput  = getContents
          , optPPrint = const $ return ()
          , optOutput = putStrLn
          , inputFileName = "stdin" }

header :: String
header = "Usage: compile [OPTION...] [FILENAME]"

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['v'] ["version"] (NoArg showVersion) "show version number"
  , Option ['i'] ["interactive"] (OptArg mkI "FILE") "start interactive shell"
  , Option ['c'] ["compile"] (ReqArg mkC "FILE") "compile MAlice code"
  , Option ['o'] ["output"] (ReqArg mkO "FILE") "output into file"
  , Option ['p'] ["pprint"] (OptArg mkPP "FILE") "pretty print AST"
  ]

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "compile Milestone 3"
  exitWith ExitSuccess

mkI :: Maybe String -> Options -> IO Options
mkI arg opt =
  return opt { optInteractive = (True, arg) }

mkC :: String -> Options -> IO Options
mkC arg opt = return opt { optInput = readFile arg
                         , inputFileName = arg }

mkO :: String -> Options -> IO Options
mkO arg opt = return opt { optOutput = writeFile arg }

mkPP :: Maybe String -> Options -> IO Options
mkPP arg opt =
  return opt { optPPrint = pAct }
  where
    pAct =
      case arg of
        Nothing -> putStrLn
        Just s  -> writeFile s

compileFile :: IO String -> (String -> IO ()) -> (String -> IO ()) -> String ->
               IO ()
compileFile input output pp file = do
  code <- input
  case compile code file pp output of
    Left err -> putStrLn err
    Right act -> act

compile :: String -> String -> (String -> IO ()) -> (String -> IO ()) -> Either String (IO ())
compile code name pp output = do
  (parsedAST, st) <- parseCode code name
  let optAST     = T.optimiseAST parsedAST
      trans      = T.renameIdentifiers optAST
      desugared  = T.desugarAST trans
      byteCode   = J.translateProgram desugared
      warn       = case MP.warnings . MP.warnList $ st of
        [] -> return ()
        _  -> putStrLn . show $ MP.warnList st
  return $
    warn                                       >>
    (output $ J.getJavaProgramString byteCode) >>
    (pp $ PP.pprintS desugared)

parseCode :: String -> String -> Either String (AST.Program, MP.ParserState)
parseCode code name =
  MP.mparse code name
