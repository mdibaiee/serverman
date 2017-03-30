module System.Serverman.Actions.Remote ( runRemotely
                                       , Address) where
  import System.Serverman.Utils
  import System.Serverman.Actions.Env

  import Data.List
  import System.Directory
  import System.IO
  import System.FilePath
  import System.Posix.Env
  import System.Posix.Files
  import Control.Monad
  import Data.Maybe
  import Control.Monad.State hiding (liftIO)
  import Control.Concurrent
  import Data.IORef
  import Data.Either

  actionDelay = 1000000

  runRemotely :: Address -> App r -> App ()
  runRemotely addr@(Address host port user) action = do
    tmp <- liftIO getTemporaryDirectory
    (Right userID) <- execute "id" ["-u"] "" True

    let servermanAddr = Address host port "serverman"
        p = if null port then [] else ["-p", port]
        connection = takeWhile (/= ':') (show addr)
        smConnection = "serverman@" ++ host
        path = tmp </> smConnection
        uid = ["-o", "uid=" ++ userID, "-o", "gid=" ++ userID]

        serverPaths = ["/usr/lib/openssh/sftp-server", "/usr/lib/ssh/sftp-server"]

        options = ["-o", "nonempty",
                   "-o", "sftp_server=sudo " ++ head serverPaths]

    home <- liftIO getHomeDirectory

    let keyPath = home </> ".ssh/serverman"
        pubPath = keyPath <.> "pub"

    liftIO $ createDirectoryIfMissing True path

    -- check if a connection to SSH server using public key is possible
    execute "fusermount" ["-u", path] "" False
    result <- execute "sshfs" (p ++ noPassword ++ uid ++ options ++ ["-o", "IdentityFile=" ++ keyPath, smConnection ++ ":/", path]) "" False

    case result of
      Right _ -> do
        state <- get
        liftIO $ threadDelay actionDelay
        put $ state { remoteMode = Just (servermanAddr, keyPath) }
        getOS
        action

        return ()

      Left err -> do
        liftIO $ print err
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

        runCommand "mkdir" ["/home/serverman/.ssh", "-p"]
        runCommand "touch" ["/home/serverman/.ssh/authorized_keys"]
        runCommand "echo" [quote publicKey, ">>", "/home/serverman/.ssh/authorized_keys"]
        runCommand "chown" ["-R", "serverman", "/home/serverman"]

        runRemotely addr action

        return ()

    return ()

    where
      noPassword = ["-o", "PasswordAuthentication=no", "-o", "PubkeyAuthentication=yes"]

      modPath path c
        | c == ':' = ":" ++ path
        | otherwise = [c]
        
