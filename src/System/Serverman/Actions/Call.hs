{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Serverman.Actions.Call (callService) where
  import System.Serverman.Types
  import System.Serverman.Utils
  import qualified System.Serverman.Actions.Repository
  import System.Serverman.Actions.Remote

  import System.Directory
  import System.FilePath
  import Language.Haskell.Interpreter hiding (get, name, liftIO)
  import Control.Monad.State hiding (liftIO)
  import System.Posix.Env
  import Data.List
  import Stack.Package

  callService :: Service -> Maybe FilePath -> App ()
  callService s@(Service { name, version }) remote = do
    state@(AppState { repositoryURL }) <- get
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

    backupEnv <- liftIO $ getEnvironment
    liftIO $ setEnvironment finalEnv

    func <- liftIO $ runInterpreter (interpreter include entry)

    case func of
      Right fn -> handleRemote remote $ fn s
      Left err -> liftIO $ do
        putStrLn $ "error reading `call` from module " ++ entry
        case err of
          WontCompile errs -> mapM_ (putStrLn . errMsg) errs

          x -> print x

    liftIO $ setEnvironment backupEnv

    return ()

    where
      handleRemote (Just file) action = do
        list <- liftIO $ map read . lines <$> readFile file
        mapM_ (`runRemotely` action) list
      handleRemote _ action = action

      mergeEnv other (key, value)
        | key `elem` ["GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOXES"] = 
          let (Just alt) = lookup key other
          in (key, value ++ ":" ++ alt)
        | otherwise = (key, value)

  interpreter :: [FilePath] -> FilePath -> Interpreter (Service -> App ())
  interpreter path entry = do
    set [searchPath := path]
    loadModules [entry]
    setTopLevelModules ["Main"]
    interpret "call" (as :: Service -> App ())

