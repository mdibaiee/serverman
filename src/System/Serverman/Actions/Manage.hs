{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Manage (startService, stopService) where
  import System.Serverman.Utils
  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Install
  import System.Serverman.Services

  import Control.Monad.State

  startService :: Service -> OS -> App ()
  startService (Service { service }) os
    | os == Mac = liftIO $ putStrLn $ "Couldn't start " ++ service ++ " automatically. If you encounter any problems, make sure it is running."
    | otherwise = executeRoot "systemctl" ["start", service] "" True
                >> execute "sleep" ["5s"] "" True
                >> return ()

  stopService :: Service -> OS -> App ()
  stopService (Service { service }) os
    | os == Mac = liftIO $ putStrLn $ "Couldn't stop " ++ service ++ " automatically."
    | otherwise = executeRoot "systemctl" ["stop", service] "" True
                >> return ()
