module System.Serverman.Actions.Install (installService, package, dependencies) where
  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Actions.Env


  import System.IO.Error
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad.Free
  import Control.Monad

  class Installable a where
    dependencies :: a -> [a]
    package :: a -> OS -> String

  instance Installable Service where
    dependencies NGINX = [LetsEncrypt]
    dependencies _ = []

    package LetsEncrypt Arch = "certbot"
    package LetsEncrypt _ = "letsencrypt"

    package NGINX _ = "nginx"

    package MySQL _ = "mysql"

    package MongoDB _ = "mongodb"

    package VsFTPd _ = "vsftpd"

    package SSHFs _ = "sshfs"

  installService :: Service -> OS -> IO ()
  installService service os = do
    forM_ (dependencies service) (`installService` os) 

    let base = case os of
          Arch -> ("pacman", ["-S", "--noconfirm", "--quiet"])
          Debian -> ("apt-get", ["install", "-y"])
          Mac -> ("brew", ["install", "-y"])
          _ -> ("echo", ["Unknown operating system"])
        pkg = package service os

    process <- async $ do
      result <- executeRoot (fst base) (snd base ++ [pkg]) "" True

      case result of
        Left err -> return ()
        Right _ -> do
          putStrLn $ "installed " ++ show service ++ "."
    wait process
