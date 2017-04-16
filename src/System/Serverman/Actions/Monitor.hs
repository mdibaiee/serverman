{-# LANGUAGE NamedFieldPuns #-}
module System.Serverman.Actions.Monitor (serviceLogs, serviceStatus) where
  import System.Serverman.Utils
  import System.Serverman.Types
  import System.Serverman.Log

  import Data.List
  import Data.Maybe
  import Control.Monad.State

  serviceStatus :: Service -> App ()
  serviceStatus s@Service { service } = do
    result <- executeRoot "systemctl" ["status", service] "" False

    AppState { remoteMode } <- get
    let addr = if isJust remoteMode then " on " ++ (show . fst . fromJust) remoteMode else ""

    case result of
      Left e
        | "Loaded: not-found" `isInfixOf` e ->
          err $ "service " ++ service ++ " was not found" ++ addr
        | "Active: inactive" `isInfixOf` e -> 
          warning $ "service " ++ service ++ " is inactive" ++ addr
        | otherwise -> 
          err $ "service " ++ service ++ " errored!" ++ addr
      Right out ->
        success $ "service " ++ service ++ " is active" ++ addr

    return ()


  serviceLogs :: Service -> App ()
  serviceLogs s@Service { service } = do
    result <- executeRoot "journalctl" ["-u", service, "--no-tail", "--no-pager"] "" False

    AppState { remoteMode } <- get
    let addr = if isJust remoteMode then " on " ++ (show . fst . fromJust) remoteMode else ""

    case result of
      Left e -> err $ "could not read service " ++ service ++ " logs: " ++ e
      Right out -> do
        success $ "service " ++ service ++ " logs " ++ addr
        write out

    return ()
