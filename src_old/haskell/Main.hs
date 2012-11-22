import Parser
import Scanner

main :: IO ()
main = do
  inputStr <- getContents
  let parseTree = parser $ scanString inputStr
  putStrLn $ "parseTree: " ++ show parseTree
  putStrLn "Done."