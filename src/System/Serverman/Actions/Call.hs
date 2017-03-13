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

  callService :: Service -> Params -> App ()
  callService s@(Service { name, version }) params = do
    state@(AppState { repositoryURL }) <- get

    dir <- liftIO $ getAppUserDataDirectory "serverman"
    let path = dir </> "repository" </> "services" </> name
        source = dir </> "source" </> "src"
        entry = path </> "src" </> "Main.hs"
        object = path </> "Main.o"

    packages <- liftIO $ readFile $ path </> "packages"

    {-result <- exec "stack" (["ghc", entry, "--package", intercalate "," . lines $ packages, "--"] ++ includeArgs) "" (Just source) True-}
    {-let packagePaths = splitAtElem packagePath ':'-}
    let include = [source, path]
        includeArgs = map ("-i"++) include

    (Right stackEnv) <- exec "stack" ["exec", "env"] "" (Just path) True

    backupEnv <- liftIO $ getEnvironment
    liftIO $ setEnvironment $ parseKeyValue stackEnv '='

    liftIO $ print include

    func <- liftIO $ runInterpreter (interpreter include entry)

    case func of
      Right fn -> fn
      Left err -> liftIO $ do
        putStrLn $ "error reading `call` from module " ++ entry
        print err

    liftIO $ setEnvironment backupEnv

    return ()
      {-result <- build entry object ["-i" ++ source]-}
      {-print result-}

      {-result :: (Maybe ) <- liftIO $ eval content ["System.Serverman.Types", "System.Serverman.Utils", "Control.Monad.State"]-}
      {-liftIO $ print result-}

  interpreter :: [FilePath] -> FilePath -> Interpreter (App ())
  interpreter path entry = do
    set [searchPath := path]
    loadModules [entry]
    setTopLevelModules ["Main"]
    interpret "call" (as :: App ())

