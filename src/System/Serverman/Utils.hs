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
                              , mkHelp
                              , indent
                              , commas
                              , quote
                              , removeTrailingNewline
                              , execIfMissing
                              , execIfExists
                              , writeFileIfMissing
                              , renameFileIfMissing
                              , appendAfter
                              , exec
                              , execute
                              , execRemote
                              , Address (..)
                              , liftIO
                              , usingPort
                              , clearPort
                              , restartService
                              , getPassword
                              , executeRoot) where

  import System.IO
  import Control.Monad
  import System.Directory
  import System.FilePath
  import System.Process
  import System.IO.Error (tryIOError)
  import Control.Concurrent.Async
  import Data.List
  import Control.Exception
  import System.Exit hiding (die)
  import System.Posix.Terminal
  import System.Posix.IO (stdInput)
  import Data.Maybe
  import System.Posix.Files
  import System.Posix.Env
  import qualified Control.Monad.State as ST
  import Control.Monad.State hiding (liftIO)
  import Data.Default.Class
  import Control.Monad.Catch (catchIOError)
  import System.Unix.Chroot
  import Control.Concurrent
  import Control.Monad.Loops

  import System.Serverman.Types
  import System.Serverman.Log
  import Debug.Trace

  -- lift IO to App, also applying remote mode and port forwarding:
  --   if in remote mode, chroot actions to the SSHFS directory
  --   forward ports declared by `usingPort`
  liftIO :: IO a -> App a
  liftIO action = do
    state@AppState { remoteMode, ports } <- get
    verbose $ "liftIO " ++ show remoteMode ++ ", " ++ show ports

    case remoteMode of
      Nothing -> ST.liftIO action

      Just rm@(Address host port user, key) -> do
        tmp <- ST.liftIO getTemporaryDirectory
        let path = tmp </> (user ++ "@" ++ host)
        
        verbose "forwarding ports"
        mapM_ (portForward rm) ports

        verbose $ "chroot directory " ++ path

        catchIOError 
          (fchroot path $ ST.liftIO action)
          (\e -> err (show e) >> ST.liftIO (threadDelay 1000000) >> liftIO action)
    where
      portForward (Address host port user, key) (source, destination) = do
        let forward = source ++ ":" ++ host ++ ":" ++ destination
            connection = user ++ "@" ++ host ++ (if null port then "" else " -p " ++ port)
            identity = " -o IdentityFile=" ++ key

        (_, _, _, handle) <- ST.liftIO $ runInteractiveCommand $ "ssh -L " ++ forward ++ " " ++ connection ++ identity

        state@AppState { processes } <- get
        put $ state { processes = handle:processes }
        return ()

  -- take and return a port from open port pool, forwarding the specified port to that port
  -- this allows connections to ports on a remote server
  usingPort :: String -> App String
  usingPort port = do
    state@AppState { ports, remoteMode } <- get

    case remoteMode of
      Nothing -> return port
      Just _ -> do
        available <- head <$> dropWhileM checkPort range

        verbose $ "using port " ++ available ++ " in place of " ++ port

        put $ state { ports = (available, port):ports }
        return available
    where
      range = map show [8000..9999]

  -- clear a port
  clearPort :: String -> App ()
  clearPort port = do
    verbose $ "freed port " ++ port
    state@AppState { ports, remoteMode } <- get
    let newPorts = filter ((/= port) . fst) ports
    put $ state { ports = newPorts }
    return ()

  -- check whether a port is open or not
  checkPort :: String -> App Bool
  checkPort port = do
    result <- execute "netstat" ["-an", "|", "grep", port] "" False
    case result of
      Left _ -> return False
      Right output ->
        if (not . null) output then
          return True
        else
          return False

  -- generates a string in format `<key><delimiter><value>\n`
  -- e.g. |keyvalue [("first", "line"), ("second", "one")] "="| outputs "first=line\nsecond=one"
  keyvalue :: [(String, String)] -> String -> String
  keyvalue ((a, b):xs) delimit = a ++ delimit ++ b ++ "\n" ++ keyvalue xs delimit
  keyvalue [] _ = ""

  -- parse a `<key><delimiter><value>` string into a list of (key, value) pairs
  parseKeyValue :: String -> Char -> [(String, String)]
  parseKeyValue text delimit = map parsePair (lines text)
    where
      parsePair line =
        let delimitIndex = fromJust $ delimit `elemIndex` line
            (key, value) = splitAt delimitIndex line
        in (key, tail value)

  -- split string at character
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

  -- add a semicolon to end of each line in string
  semicolon :: String -> String
  semicolon text = unlines $ map (++ ";") (lines text)

  -- create a block with the following format: `<name> {\n<content>\n}`
  -- content is |indent|ed
  block :: String -> String -> String
  block blockName content = blockName ++ " {\n" ++ indent content ++ "}"

  -- alias for |intercalate ", "|
  commas :: [String] -> String
  commas = intercalate ", "

  -- execute an action if a path is missing
  execIfMissing :: (Applicative f, Monad f, MonadIO f) => FilePath -> f () -> f ()
  execIfMissing path action = do
    exists <- ST.liftIO $ doesPathExist path
    
    unless exists action

  -- execute an action if a path exists
  execIfExists :: (Applicative f, Monad f, MonadIO f) => FilePath -> f () -> f ()
  execIfExists path action = do
    exists <- ST.liftIO $ doesPathExist path
    
    when exists action

  writeFileIfMissing :: FilePath -> String -> IO ()
  writeFileIfMissing path content = execIfMissing path (writeFile path content)

  renameFileIfMissing :: FilePath -> String -> IO ()
  renameFileIfMissing path content = execIfMissing content (renameFile path content)

  -- append a line after a specific string
  appendAfter :: String -> String -> String -> String
  appendAfter content after line =
    let ls = lines content
        appended = concatMap (\x -> if x == after then [x, line] else [x]) ls

    in unlines appended

  -- indent all lines forward using \t
  indent :: String -> String
  indent s = unlines $ map ("\t" ++) (lines s)

  -- put single quotes around a text
  quote :: String -> String
  quote input = "'" ++ input ++ "'"

  removeTrailingNewline :: String -> String
  removeTrailingNewline input
    | (reverse . take 1 . reverse) input == "\n" = take (length input - 1) input
    | otherwise = input

  execute :: String -> [String] -> String -> Bool -> App (Either String String)
  execute cmd args stdin = exec cmd args stdin Nothing

  -- execute a command in operating system
  -- if in remote mode, runs `execRemote`
  exec :: String -> [String] -> String -> Maybe FilePath -> Bool -> App (Either String String)
  exec cmd args stdin cwd logErrors = do
    verbose $ "exec: " ++ cmd ++ " " ++ show args
    AppState { remoteMode } <- get

    if isJust remoteMode then do
      let (addr, key) = fromJust remoteMode

      execRemote addr (Just key) Nothing "" cmd args stdin cwd logErrors
    else do
      let command = escape $ cmd ++ " " ++ unwords args
          cp = (proc (escape cmd) (map escape args)) { cwd = cwd }

      verbose $ "executing command |" ++ command ++ "|"

      result <- ST.liftIO . tryIOError $ readCreateProcessWithExitCode cp stdin
      verbose "command executed"

      case result of
        Right (ExitSuccess, stdout, _) -> do
          verbose $ "command successful: " ++ stdout
          return $ Right stdout

        Right (ExitFailure code, stdout, stderr) -> do
          unless logErrors $ verbose $ "command failed: " ++ show code ++ ", stderr: " ++ stderr
          when logErrors $ do
            err command
            err $ "exit code: " ++ show code
            err stdout
            err stderr
          return $ Left stdout
        Left e -> do
          unless logErrors $ verbose $ "couldn't execute command: " ++ show e
          when logErrors $ do
            err command
            err $ show e
          return $ Left (show e)

    where
      escape :: String -> String
      escape string = foldl' (\str char -> replace str char ('\\':char)) string specialCharacters
        where
          specialCharacters = ["$"]

  -- run a command on a server using SSH
  execRemote :: Address -> Maybe String -> Maybe String -> String -> String -> [String] -> String -> Maybe String -> Bool -> App (Either String String)
  execRemote addr@(Address host port user) maybeKey maybeUser password cmd args stdin cwd logErrors = do
    tmp <- ST.liftIO getTemporaryDirectory
    let passwordFile = tmp </> "pw"

    let userArgument = case maybeUser of
                         Just user -> if (not . null) password then
                                        ["echo", password, "|", "sudo", "-S", "-u", user]
                                      else
                                        ["sudo", "-u", user]
                         Nothing -> []
        keyArgument = case maybeKey of
                        Just key -> 
                          ["-o", "IdentityFile=" ++ key] ++ noPassword
                        Nothing -> noKey

        p = if null port then [] else ["-p", port]
        connection = takeWhile (/= ':') (show addr)

        cumulated = p ++ keyArgument ++ options
        command = userArgument ++ ["sh -c \"", cmd] ++ args ++ ["\""]
        complete = "-w" : "ssh" : (cumulated ++ [connection] ++ intersperse " " command)

    verbose "backing up environment variables"
    backupEnv <- ST.liftIO getEnvironment

    unless (null password) $ do
      verbose $ "writing passwordFile for SSH " ++ passwordFile ++ " and setting SSH_ASKPASS"
      ST.liftIO $ do
        writeFile passwordFile $ "echo " ++ password
        setFileMode passwordFile accessModes
        setEnv "SSH_ASKPASS" passwordFile True

    state <- get
    let AppState { remoteMode = backup } = state
    put $ state { remoteMode = Nothing }

    verbose $ "executing command |setsid " ++ show complete ++ "|"

    result <- exec "setsid" complete stdin cwd logErrors
    put $ state { remoteMode = backup }

    verbose "reseting environment and deleting password file"
    ST.liftIO $ do
      setEnvironment backupEnv
      execIfExists passwordFile $ removeFile passwordFile

    return result
    where
      noPassword = ["-o", "PasswordAuthentication=no", "-o", "PubkeyAuthentication=yes"]
      noKey = ["-o", "PubkeyAuthentication=no", "-o", "PasswordAuthentication=yes"]
      options = ["-o", "StrictHostKeyChecking=no"]

  -- replace in string
  replace :: String -> String -> String -> String
  replace str replacable alt =
    foldl' rep "" str
    where
      rep acc n
          | takeEnd (l - 1) acc ++ [n] == replacable = dropEnd (l - 1) acc ++ alt
          | otherwise = acc ++ [n]

      l = length replacable
      takeEnd n = reverse . take n . reverse
      dropEnd n = reverse . drop n . reverse

  restartService :: String -> App (Either String String)
  restartService service = do
    verbose $ "restarting service " ++ service
    executeRoot "systemctl" ["restart", service] "" True

  -- execute using sudo
  executeRoot :: String -> [String] -> String -> Bool -> App (Either String String)
  executeRoot cmd args = execute "sudo" (cmd:args)

  -- read password from user input (don't show the input)
  getPassword :: IO String
  getPassword = do
    tc <- getTerminalAttributes stdInput
    setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
    password <- getLine
    setTerminalAttributes stdInput tc Immediately
    return password

  -- make tabularized help string
  mkHelp :: String -> [(String, String)] -> String
  mkHelp name entries = name ++ "\n" ++ 
                          indent (keyvalue tabularized " ")
    where
      maxKey = maximum $ map (length . fst) entries
      tabularized = map (\(key, value) -> (key ++ replicate (maxKey - length key + 1) ' ', value)) entries
