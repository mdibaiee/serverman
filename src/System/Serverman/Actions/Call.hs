{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Serverman.Actions.Call (callService) where
  import System.Serverman.Types
  import System.Serverman.Utils
  import qualified System.Serverman.Actions.Repository

  import System.Directory
  import System.FilePath
  import Language.Haskell.Interpreter hiding (get, name)
  import Control.Monad.State
  import System.Posix.Env
  import Data.List
  import Stack.Package

  callService :: Service -> App ()
  callService s@(Service { name, version }) = do
    state@(AppState { repositoryURL }) <- get

    dir <- liftIO $ getAppUserDataDirectory "serverman"
    let path = dir </> "repository" </> "services" </> name
        source = dir </> "source" </> "src"
        src = path </> "src"
        entry = src </> "Main.hs"

    let include = [source, src]
        includeArgs = map ("-i"++) include

    (Right stackEnv) <- exec "stack" ["install", "--dependencies-only"] "" (Just path) True
    (Right stackEnv) <- exec "stack" ["exec", "env"] "" (Just path) True

    backupEnv <- liftIO $ getEnvironment
    liftIO $ setEnvironment $ parseKeyValue stackEnv '='

    func <- liftIO $ runInterpreter (interpreter include entry)

    case func of
      Right fn -> fn s
      Left err -> liftIO $ do
        putStrLn $ "error reading `call` from module " ++ entry
        case err of
          WontCompile errs -> mapM_ (putStrLn . errMsg) errs

          x -> print x

    liftIO $ setEnvironment backupEnv

    return ()

  interpreter :: [FilePath] -> FilePath -> Interpreter (Service -> App ())
  interpreter path entry = do
    set [searchPath := path]
    loadModules [entry]
    setTopLevelModules ["Main"]
    interpret "call" (as :: Service -> App ())

