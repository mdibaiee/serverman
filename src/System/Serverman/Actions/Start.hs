module System.Serverman.Actions.Start (startService) where
  import System.Serverman.Utils
  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Install
  import System.Serverman.Services

  startService :: Service -> OS -> IO ()
  startService service os
    | os == Mac = putStrLn $ "Couldn't start " ++ package service os ++ " automatically. If you encounter any problems, make sure it is running."
    | otherwise = executeRoot "systemctl" ["start", package service os] "" True
                >> execute "sleep" ["5s"] "" True
                >> return ()
