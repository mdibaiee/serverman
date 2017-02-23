module System.Serverman.Utils ( keyvalue
                              , block
                              , indent
                              , writeFileIfMissing
                              , commandError
                              , appendAfter
                              , execute
                              , executeRoot) where

  import System.IO
  import Control.Monad
  import System.Directory
  import System.Process
  import System.IO.Error
  import Control.Concurrent.Async
  import Data.List
  import Control.Exception
  import System.Exit

  keyvalue :: [(String, String)] -> String
  keyvalue ((a, b):xs) = a ++ " " ++ b ++ ";\n" ++ keyvalue xs
  keyvalue [] = ""

  block :: String -> String -> String
  block blockName content = blockName ++ " {\n" ++ indent content ++ "}"

  writeFileIfMissing :: FilePath -> String -> IO ()
  writeFileIfMissing path content = do
    exists <- doesFileExist path
    
    when (not exists) $ do
      writeFile path content

  appendAfter :: String -> String -> String -> String
  appendAfter content after line =
    let ls = lines content
        appended = concat $ map (\x -> if x == after then [x, line] else [x]) ls

    in unlines appended

  indent :: String -> String
  indent s = unlines $ map ("\t" ++) (lines s)

  commandError :: String -> String
  commandError command = "[Error] an error occured while running: " ++ command ++ "\nplease try running the command manually."

  execute :: String -> [String] -> String -> Bool -> IO (Either String String)
  execute cmd args stdin logErrors = do
    let command = cmd ++ " " ++ intercalate " " args

    process <- async $ do
      result <- tryIOError $ readProcessWithExitCode cmd args stdin

      case result of
         Right (ExitSuccess, stdout, _) -> return $ Right stdout

         Right (ExitFailure code, stdout, stderr) -> do
           when logErrors $ do
             putStrLn $ "exit code: " ++ show code
             putStrLn stdout
             putStrLn stderr
             putStrLn $ commandError command
           return $ Left stdout
         Left err -> do
           when logErrors $ do
             putStrLn $ show err
             putStrLn $ commandError command
           return $ Left (show err)

    wait process

  executeRoot :: String -> [String] -> String -> Bool -> IO (Either String String)
  executeRoot cmd args stdin logErrors = execute "sudo" (cmd:args) stdin logErrors
