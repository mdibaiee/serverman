{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Install (installService) where
  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services hiding (info)
  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Repository
  import System.Serverman.Types
  import System.Serverman.Log

  import System.IO.Error
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.State hiding (liftIO)
  import Control.Monad.Trans.Control
  import Data.List
  import Data.Maybe

  installService :: Service -> App ()
  installService s@(Service { dependencies, packages }) = do
    done <- progressText $ "installing " ++ show s
    (AppState { os }) <- get

    deps <- catMaybes <$> mapM findService dependencies
    forM_ deps installService 

    let base = case os of
          Arch -> ("pacman", ["-S", "--noconfirm", "--quiet"])
          Debian -> ("apt-get", ["install", "-y"])
          _ -> ("echo", ["Unknown operating system"])
        pkg = packageByOS s os

    result <- executeRoot (fst base) (snd base ++ pkg) "" True
    done

    case result of
      Left err -> return ()
      Right _ -> info $ "installed " ++ show s

    return ()
