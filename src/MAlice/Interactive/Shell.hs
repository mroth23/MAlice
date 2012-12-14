module MAlice.Interactive.Shell where

import qualified Data.Map as M
import Control.Monad.State
import System.IO
import System.Exit
import MAlice.Language.AST
import MAlice.Interactive.Parser
import MAlice.Interactive.Types
import MAlice.Interactive.Eval
import MAlice.Parser.Parser
import MAlice.Parser.ParserState

main :: Maybe String -> IO ()
main s = do
  putStrLn "MAlice interactive shell version M3.00, type :? for help"
  putStrLn "--------------------------------------------------------"
  is <- maybe (return rInitSt) (flip execStateT rInitSt . loadFile) s
  evalStateT userInputLoop is

rInitSt = (RuntimeState initState (Program $ DeclList []) [M.empty] "")

--Executing this with evalStateT will yield an IO () action
--liftIO lifts IO actions to the MEval monad (which is an instance of MonadIO)
userInputLoop :: MEval ()
userInputLoop = do
  liftIO $ putStr ">" >> hFlush stdout
  input <- liftIO getLine
  pst <- getParserState
  case parseUserInput pst input of
    Left err -> (liftIO $ putStrLn err) >> userInputLoop
    Right (i, pst') ->
      handleInput i >>
      (modifyState $ \st -> st { parserState = resetErrors pst' }) >>
      userInputLoop

resetErrors :: ParserState -> ParserState
resetErrors st = st { errorList = SemanticErrors [] }

handleInput :: UserInput -> MEval ()
handleInput (Command Quit) = liftIO . exitWith $ ExitSuccess
handleInput (Command Help) = liftIO . putStrLn $ helpMessage
handleInput (Command ReloadFile) = getCurrentFile >>= loadFile
handleInput (Command (LoadFile f)) = loadFile f
handleInput (EvalDecl d) = runDecl d
handleInput (EvalExpr e) = evalExpr e >>= (liftIO . putStrLn . show)
handleInput (EvalStmt c) = runCompoundStmt c >> return ()

loadFile :: String -> MEval ()
loadFile f = do
  code <- safeRead f
  maybe (liftIO . putStrLn $ "Invalid file: " ++ f) (parseCode f) code

parseCode :: String -> String -> MEval ()
parseCode f code = do
  case mparse code f of
    Left err -> liftIO . putStrLn $ err
    Right (ast, pst) -> do
      liftIO . putStrLn $ "Loaded " ++ f
      modifyState $ \st -> st { parserState = pst
                              , programAST  = ast
                              , currentFile = f }
      evalGlobals ast

-- If reading the file fails, return Nothing
safeRead :: String -> MEval (Maybe String)
safeRead f =
  liftIO (catch (readFile f >>= return . Just) (const $ return Nothing))

helpMessage = ":l [FILE] to load a file\n" ++
              ":r to reload the current file\n" ++
              ":q to quit\n" ++
              "or any expression, declaration or statement to execute"