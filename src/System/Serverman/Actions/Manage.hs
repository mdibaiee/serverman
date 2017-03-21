{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Manage (startService, stopService) where
  import System.Serverman.Types
  import System.Serverman.Utils
  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Install
  import System.Serverman.Services

  import Control.Monad.State hiding (liftIO)

  startService :: Service -> App ()
  startService (Service { service }) = do
    (AppState { os }) <- get
    case os of 
      _   -> do
        executeRoot "systemctl" ["start", service] "" True
        execute "sleep" ["5s"] "" True
        return ()

  stopService :: Service -> App ()
  stopService (Service { service }) = do
    (AppState { os }) <- get
    case os of 
      _   -> do
        executeRoot "systemctl" ["stop", service] "" True
        return ()
