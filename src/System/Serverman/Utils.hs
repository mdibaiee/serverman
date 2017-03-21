{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Utils ( App (..)
                              , AppState (..)
                              , runApp
                              , keyvalue
                              , parseKeyValue
                              , splitAtElem
                              , semicolon
                              , block
                              , indent
                              , commas
                              , quote
                              , removeTrailingNewline
                              , execIfMissing
                              , execIfExists
                              , writeFileIfMissing
                              , renameFileIfMissing
                              , commandError
                              , appendAfter
                              , exec
                              , execute
                              , execRemote
                              , Address (..)
                              , liftedAsync
                              , liftIO
                              , restartService
                              , getPassword
                              , executeRoot) where

  import System.IO
  import Control.Monad
  import System.Directory
  import System.FilePath
  import System.Process
  import System.IO.Error
  import Control.Concurrent.Async
  import Data.List
  import Control.Exception
  import System.Exit
  import Data.Maybe
  import System.Posix.Terminal
  import System.Posix.IO (stdInput)
  import Data.Maybe
  import System.Posix.Files
  import System.Posix.Env
  import qualified Control.Monad.State as ST
  import Control.Monad.State hiding (liftIO)
  import Control.Monad.Trans.Control
  import Data.Default.Class
  import System.Unix.Chroot
  import Control.Monad.Catch

  import System.Serverman.Types

  liftIO :: (MonadIO m, MonadState AppState m, MonadMask m) => IO a -> m a
  {-liftIO :: IO a -> App a-}
  liftIO action = do
    state@(AppState { remoteMode }) <- get

    case remoteMode of
      Nothing -> ST.liftIO action

      Just (Address host port user, _) -> do
        tmp <- ST.liftIO getTemporaryDirectory
        let path = tmp </> (user ++ "@" ++ host)
        
        fchroot path $ ST.liftIO action

  keyvalue :: [(String, String)] -> String -> String
  keyvalue ((a, b):xs) delimit = a ++ delimit ++ b ++ "\n" ++ keyvalue xs delimit
  keyvalue [] _ = ""

  parseKeyValue :: String -> Char -> [(String, String)]
  parseKeyValue text delimit = map parsePair (lines text)
    where
      parsePair line =
        let delimitIndex = fromJust $ delimit `elemIndex` line
            (key, value) = splitAt delimitIndex line
        in (key, tail value)

  splitAtElem :: String -> Char -> [String]
  splitAtElem "" _ = []
  splitAtElem str char =
    case charIndex of
      Just index -> 
        let (left, x:right) = splitAt index str
        in left : splitAtElem right char
      Nothing -> [str]
    where
      charIndex = char `elemIndex` str

  semicolon :: String -> String
  semicolon text = unlines $ map (++ ";") (lines text)

  block :: String -> String -> String
  block blockName content = blockName ++ " {\n" ++ indent content ++ "}"

  commas :: [String] -> String
  commas text = intercalate ", " text

  execIfMissing :: (Applicative f, Monad f, MonadIO f) => FilePath -> f () -> f ()
  execIfMissing path action = do
    exists <- ST.liftIO $ doesPathExist path
    
    when (not exists) action

  execIfExists :: (Applicative f, Monad f, MonadIO f) => FilePath -> f () -> f ()
  execIfExists path action = do
    exists <- ST.liftIO $ doesPathExist path
    
    when exists action

  writeFileIfMissing :: FilePath -> String -> IO ()
  writeFileIfMissing path content = execIfMissing path (writeFile path content)

  renameFileIfMissing :: FilePath -> String -> IO ()
  renameFileIfMissing path content = execIfMissing content (renameFile path content)

  appendAfter :: String -> String -> String -> String
  appendAfter content after line =
    let ls = lines content
        appended = concat $ map (\x -> if x == after then [x, line] else [x]) ls

    in unlines appended

  indent :: String -> String
  indent s = unlines $ map ("\t" ++) (lines s)

  quote :: String -> String
  quote input = "'" ++ input ++ "'"

  removeTrailingNewline :: String -> String
  removeTrailingNewline input
    | (reverse . take 1 . reverse) input == "\n" = take (length input - 1) input
    | otherwise = input

  commandError :: String -> String
  commandError command = "[Error] an error occured while running: " ++ command ++ "\nplease try running the command manually."

  execute :: String -> [String] -> String -> Bool -> App (Either String String)
  execute cmd args stdin logErrors = exec cmd args stdin Nothing logErrors

  exec :: String -> [String] -> String -> Maybe FilePath -> Bool -> App (Either String String)
  exec cmd args stdin cwd logErrors = do
    (AppState { remoteMode }) <- get

    if isJust remoteMode then do
      let (addr, key) = fromJust remoteMode

      execRemote addr (Just key) (Just "serverman") "" cmd args stdin cwd logErrors
    else liftIO $ do
      let command = escape $ cmd ++ " " ++ intercalate " " args
          cp = (proc (escape cmd) (map escape args)) { cwd = cwd }

      process <- async $ do
        result <- tryIOError $ readCreateProcessWithExitCode cp stdin

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

    where
      escape :: String -> String
      escape string = foldl' (\str char -> replace str char ('\\':char)) string specialCharacters
        where
          specialCharacters = ["$"]

  execRemote :: Address -> Maybe String -> Maybe String -> String -> String -> [String] -> String -> Maybe String -> Bool -> App (Either String String)
  execRemote addr@(Address host port user) maybeKey maybeUser password cmd args stdin cwd logErrors = do
    tmp <- liftIO getTemporaryDirectory
    let passwordFile = tmp </> "pw"

    let userArgument = if isJust maybeUser then ["echo", password, "|", "sudo -S", "-u", fromJust maybeUser] else []
        keyArgument = if isJust maybeKey then ["-o", "IdentityFile=" ++ fromJust maybeKey] ++ noPassword else noKey
        p = if null port then [] else ["-p", port]
        connection = takeWhile (/= ':') (show addr)

        cumulated = p ++ keyArgument ++ options
        command = userArgument ++ ["sh -c \"", cmd] ++ args ++ ["\""]

    (backupEnv, passwordFile) <- liftIO $ do
      backupEnv <- getEnvironment 

      writeFile passwordFile $ "echo " ++ password
      setFileMode passwordFile accessModes
      setEnv "SSH_ASKPASS" passwordFile True

      return (backupEnv, passwordFile)

    state <- get
    let (AppState { remoteMode = backup }) = state
    put $ state { remoteMode = Nothing }
    result <- exec "setsid" ("ssh" : cumulated ++ [connection] ++ command) stdin cwd logErrors
    put $ state { remoteMode = backup }

    liftIO $ do
      setEnvironment backupEnv
      removeFile passwordFile

    return result
    where
      noPassword = ["-o", "PasswordAuthentication=no", "-o", "PubkeyAuthentication=yes"]
      noKey = ["-o", "PubkeyAuthentication=no", "-o", "PasswordAuthentication=yes"]
      options = ["-o", "StrictHostKeyChecking=no"]

  replace :: String -> String -> String -> String
  replace str replacable alt =
    foldl' rep "" str
    where
      rep acc n
          | takeEnd (l - 1) acc ++ [n] == replacable = (dropEnd (l - 1) acc) ++ alt
          | otherwise = acc ++ [n]

      l = length replacable
      takeEnd n = reverse . take n . reverse
      dropEnd n = reverse . drop n . reverse

  restartService :: String -> App (Either String String)
  restartService service = executeRoot "systemctl" ["restart", service] "" True

  executeRoot :: String -> [String] -> String -> Bool -> App (Either String String)
  executeRoot cmd args stdin logErrors = execute "sudo" (cmd:args) stdin logErrors

  getPassword :: IO String
  getPassword = do
    tc <- getTerminalAttributes stdInput
    setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
    password <- getLine
    setTerminalAttributes stdInput tc Immediately
    return password

  liftedAsync :: MonadBaseControl IO m => m a -> m (Async (StM m a))
  liftedAsync m = liftBaseWith $ \runInIO -> async (runInIO m)
