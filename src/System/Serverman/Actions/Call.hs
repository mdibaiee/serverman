{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Serverman.Actions.Call (callService) where
  import System.Serverman.Types
  import System.Serverman.Utils hiding (liftIO)
  import System.Serverman.Log
  import qualified System.Serverman.Actions.Repository
  import System.Serverman.Actions.Remote

  import System.Directory
  import System.FilePath
  import Language.Haskell.Interpreter hiding (get, name, liftIO)
  import Control.Monad.State
  import System.Posix.Env
  import Data.List
  import Stack.Package
  import Data.Maybe

  callService :: Service -> Maybe FilePath -> App ()
  callService s@Service { name, version } remote = do
    done <- progressText $ "running service " ++ show s

    state@AppState { repositoryURL, helpArg } <- get
    put $ state { remoteMode = Nothing }

    dir <- liftIO $ getAppUserDataDirectory "serverman"
    let path = dir </> "repository" </> "services" </> name
        source = dir </> "source" </> "src"
        src = path </> "src"
        entry = src </> "Main.hs"

    let include = [source, src]
        includeArgs = map ("-i"++) include

    exec "stack" ["setup", "--allow-different-user"] "" (Just path) True
    exec "stack" ["install", "--dependencies-only", "--allow-different-user"] "" (Just path) True
    exec "stack" ["install", "--dependencies-only", "--allow-different-user"] "" (Just source) True

    (Right stackEnv) <- exec "stack" ["exec", "env", "--allow-different-user"] "" (Just path) True
    (Right stackSourceEnv) <- exec "stack" ["exec", "env", "--allow-different-user"] "" (Just source) True

    let finalEnv = map (mergeEnv $ parseKeyValue stackSourceEnv '=') (parseKeyValue stackEnv '=')

    backupEnv <- liftIO getEnvironment
    liftIO $ setEnvironment finalEnv

    func <- liftIO $ runInterpreter (getCall include entry)
    helpOutput <- liftIO $ runInterpreter (getHelp include entry)

    done

    if helpArg then
      case helpOutput of
        Right fn -> write =<< fn
        Left e -> do
          write $ "could not find a help entry for " ++ name
          case e of
            WontCompile errs -> mapM_ (write . errMsg) errs

            GhcException ie -> err ie
            UnknownError ie -> err ie
            NotAllowed ie -> err ie
    else
      case func of
        Right fn -> handleRemote remote $ fn s
        Left e -> do
          err $ "couldn't read `call` from module " ++ entry
          case e of
            WontCompile errs -> mapM_ (write . errMsg) errs

            GhcException ie -> err ie
            UnknownError ie -> err ie
            NotAllowed ie -> err ie

    liftIO $ setEnvironment backupEnv

    return ()

    where
      handleRemote (Just file) action = do
        list <- liftIO $ map read . filter (not . null) . lines <$> readFile file
        mapM_ (`runRemotely` action) list
      handleRemote _ action = action

      mergeEnv other (key, value)
        | key `elem` ["GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOXES", "PATH"] = 
          let (Just alt) = lookup key other
          in (key, value ++ ":" ++ alt)
        | key == "LD_PRELOAD" = (key, "")
        | otherwise = (key, value)

  getCall :: [FilePath] -> FilePath -> Interpreter (Service -> App ())
  getCall path entry = do
    initializeInterpreter path entry
    interpret "call" (as :: Service -> App ())

  getHelp :: [FilePath] -> FilePath -> Interpreter (App String)
  getHelp path entry = do
    initializeInterpreter path entry
    interpret "help" (as :: App String)

  initializeInterpreter path entry = do
    set [searchPath := path]
    loadModules [entry]
    setTopLevelModules ["Main"]
