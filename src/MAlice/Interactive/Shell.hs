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

-- Loads the file (if any) and runs the input loop
main :: Maybe String -> IO ()
main s = do
  putStrLn "MAlice interactive shell version M3.01, type :? for help"
  putStrLn "--------------------------------------------------------"
  is <- maybe (return rInitSt) (loadAndRun) s
  evalStateT userInputLoop is

loadAndRun :: String -> IO RuntimeState
loadAndRun s = do
  st <- execStateT (loadFile s) rInitSt
  execStateT (runMExec $ evalGlobals_) st

evalGlobals_ :: MExec ()
evalGlobals_ = do
  st <- lift get
  evalGlobals (programAST st)

rInitSt = (RuntimeState initState (Program $ DeclList []) [M.empty] "")

--Executing this with evalStateT will yield an IO () action
--Loops to take user input and evaluate it
userInputLoop :: MEval ()
userInputLoop = do
  liftIO $ putStr "\n>" >> hFlush stdout
  input <- liftIO getLine
  pst <- getParserState
  case parseUserInput pst input of
    Left err -> (liftIO $ putStrLn err) >> userInputLoop
    Right (i, pst') -> do
      inputRes <- runMExec $ handleInput i
      case inputRes of
        Left err -> (liftIO $ putStrLn err) >> userInputLoop
        Right _ -> do
          modifyState $ \st -> st { parserState = resetErrors pst' }
          userInputLoop

resetErrors :: ParserState -> ParserState
resetErrors st = st { errorList = SemanticErrors [] }

-- Evaluates the different kinds of user input
handleInput :: UserInput -> MExec ()
handleInput (Command Quit) =
  liftIO . exitWith $ ExitSuccess
handleInput (Command Help) =
  liftIO . putStrLn $ helpMessage
handleInput (Command ReloadFile) = do
  cf <- lift $ getCurrentFile
  lift $ loadFile cf
  evalGlobals_
handleInput (Command (LoadFile f)) = do
  lift $ loadFile f
  evalGlobals_
handleInput (EvalDecl d) =
  runDecl d
handleInput (EvalExpr e) =
  evalExpr e >>= (liftIO . putStrLn . show)
handleInput (EvalStmt c) =
  runCompoundStmt c >> return ()

--Loads a file into "memory"
loadFile :: String -> MEval ()
loadFile f = do
  code <- safeRead f
  maybe (liftIO . putStrLn $ "Invalid file: " ++ f) (parseCode f) code

--Does the actual parsing of the file, and clears runtime state
parseCode :: String -> String -> MEval ()
parseCode f code = do
  case mparse code f of
    Left err -> liftIO . putStrLn $ err
    Right (ast, pst) -> do
      liftIO . putStrLn $ "Loaded " ++ f
      modifyState $ \st -> st { parserState = pst
                              , programAST  = ast
                              , currentFile = f
                              , memory = [M.empty] }


-- If reading the file fails, return Nothing
safeRead :: String -> MEval (Maybe String)
safeRead f =
  liftIO (catch (readFile f >>= return . Just) (const $ return Nothing))

helpMessage = ":l [FILE] to load a file\n" ++
              ":r to reload the current file\n" ++
              ":q to quit\n" ++
              "or any expression, declaration or statement to execute"