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
    package :: a -> OS -> String

  instance Installable Service where
    dependencies _ = []

    package NGINX _ = "nginx"
    package MySQL _ = "mysql"

  installService :: Service -> OS -> IO ()
  installService service os = do
    let base = case os of
          Arch -> ("pacman", ["-S", "--noconfirm", "--quiet"])
          Debian -> ("apt-get", ["install", "-y"])
          Mac -> ("brew", ["install", "-y"])
          _ -> ("echo", ["Unknown operating system"])
        pkg = package service os


    process <- async $ do
      result <- execute (fst base) (snd base ++ [pkg]) "" True

      case result of
        Left err -> return ()
        Right stdout -> do
          putStrLn stdout
          putStrLn $ "installed " ++ show service ++ "."
    wait process
