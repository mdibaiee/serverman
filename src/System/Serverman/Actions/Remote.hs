module System.Serverman.Actions.Remote ( runRemotely
                                       , Address) where
  import System.Serverman.Utils

  import System.Unix.Chroot
  import Data.List
  import System.Directory
  import System.IO
  import System.FilePath
  import System.Posix.Env
  import System.Posix.Files
  import Control.Monad
  import Data.Maybe
  import Data.IORef
  import Control.Monad.State

  import Debug.Trace

  runRemotely :: Address -> App r -> App ()
  runRemotely addr@(Address host port user) action = do
    let servermanAddr = Address host port "serverman"
        p = if null port then [] else ["-p", port]
        connection = takeWhile (/= ':') (show addr)
        smConnection = "serverman@" ++ host
        path = "/tmp/serverman/" </> connection

    home <- liftIO getHomeDirectory

    let keyPath = home </> ".ssh/serverman"
        pubPath = keyPath <.> "pub"

    liftIO $ createDirectoryIfMissing True path

    execute "fusermount" ["-u", path] "" False

    let sftpOptions = ["-o", "sftp_server=sudo -u serverman /usr/lib/openssh/sftp-server"]

    result <- execute "sshfs" (p ++ noPassword ++ sftpOptions ++ ["-o", "nonempty", "-o", "IdentityFile=" ++ keyPath, smConnection ++ ":/", path]) "" True

    case result of
      Right _ -> do
        state <- get
        put $ state { remoteMode = Just (servermanAddr, keyPath) }
        action

        return ()

      Left _ -> do
        liftIO $ do
          putStrLn $ "it seems to be the first time you are using serverman for configuring " ++ show addr
          putStrLn $ "remotely. serverman will create a user, and add it to sudoers file. an ssh key will be created"
          putStrLn $ "and that will be used for connecting to the server from now on."
          putStrLn $ "you might be prompted for password if you are not using SSH key authentication."

          putStrLn $ "Enter password for " ++ connection

        home <- liftIO getHomeDirectory
        password <- liftIO getPassword

        execIfMissing keyPath $ execute "ssh-keygen" ["-N", "", "-f", keyPath] "" True >> return ()

        publicKey <- liftIO $ readFile pubPath

        let runCommand a b = execRemote addr Nothing (Just "root") password a b "" Nothing True
            runServerman a b = execRemote addr (Just keyPath) (Just "serverman") password a b "" Nothing True

        (Right encryptedPassword) <- execute "openssl" ["passwd", "-1", "serverman"] "" True
        runCommand "useradd" ["-m", "-p", (quote . removeTrailingNewline) encryptedPassword, "serverman"]
        runCommand "echo" ["'serverman ALL=(ALL) NOPASSWD: ALL'", ">>", "/etc/sudoers"]

        runServerman "mkdir" ["/home/serverman/.ssh", "-p"]
        runServerman "touch" ["/home/serverman/.ssh/authorized_keys"]
        runServerman "echo" [quote publicKey, ">>", "/home/serverman/.ssh/authorized_keys"]

        return ()

    return ()

    where
      noPassword = ["-o", "PasswordAuthentication=no", "-o", "PubkeyAuthentication=yes"]

      chroot path (key, value)
        | key == "PATH" = (key, path ++ concatMap (modPath path) value)
        | otherwise = (key, value)

      modPath path c
        | c == ':' = ":" ++ path
        | otherwise = [c]
        
