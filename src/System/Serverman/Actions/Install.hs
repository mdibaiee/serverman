module System.Serverman.Actions.Install (installService) where
  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Actions.Env


  import System.IO.Error
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad.Free

  class Installable a where
    dependencies :: a -> [String]
    package :: a -> String

  instance Installable Service where
    dependencies _ = []

    package NGINX = "nginx"
    package Apache = "apache2"

  installService :: Service -> OS -> IO ()
  installService service os = do
    let command = case os of
          Arch -> "pacman -S "
          Debian -> "apt-get install "
          Mac -> "brew install "
          _ -> "echo 'Unknown operating system'"

    process <- async $ do
      result <- tryIOError $ callCommand (command ++ package service)

      case result of
        Left err ->
          putStrLn $ commandError command
        Right _ ->
          putStrLn $ "installed " ++ show service ++ "."
    wait process
