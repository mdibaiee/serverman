{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Install (installService) where
  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Repository
  import System.Serverman.Types

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
    (AppState { os }) <- get

    deps <- catMaybes <$> mapM findService dependencies
    forM_ deps installService 

    let base = case os of
          Arch -> ("pacman", ["-S", "--noconfirm", "--quiet"])
          Debian -> ("apt-get", ["install", "-y"])
          _ -> ("echo", ["Unknown operating system"])
        pkg = packageByOS s os

    process <- liftedAsync $ do
      result <- executeRoot (fst base) (snd base ++ pkg) "" True

      case result of
        Left err -> return ()
        Right _ -> do
          liftIO $ putStrLn $ "installed " ++ show s ++ "."
      
    liftIO $ wait process
    return ()
