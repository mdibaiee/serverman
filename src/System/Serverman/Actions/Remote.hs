{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Remote ( runRemotely
                                       , Address
                                       , unmountPath) where
  import System.Serverman.Utils hiding (liftIO)
  import System.Serverman.Actions.Env
  import System.Serverman.Log

  import Data.List
  import System.Directory
  import System.IO
  import System.FilePath
  import System.Posix.Env
  import System.Posix.Files
  import System.Posix.Types
  import Control.Monad
  import Data.Maybe
  import Control.Monad.State
  import Control.Concurrent
  import Data.IORef
  import Data.Either

  actionDelay = 1000000

  unmountPath :: Address -> App ()
  unmountPath addr@(Address host port user) = do
    tmp <- liftIO getTemporaryDirectory

    let path = tmp </> ("serverman@" ++ host)

    execute "fusermount" ["-u", path] "" False
    return ()

  runRemotely :: Address -> App r -> App (Either String r)
  runRemotely addr@(Address host port user) action = do
    verbose $ "running action remotely on " ++ show addr
    done <- progressText $ "connecting to server " ++ show addr

    tmp <- liftIO getTemporaryDirectory
    (Right userID) <- execute "id" ["-u"] "" True

    let servermanAddr = Address host port "serverman"
        p = if null port then [] else ["-p", port]
        connection = takeWhile (/= ':') (show addr)
        smConnection = "serverman@" ++ host
        path = tmp </> smConnection
        uid = ["-o", "uid=" ++ removeTrailingNewline userID, "-o", "gid=" ++ removeTrailingNewline userID]

        serverPaths = ["/usr/lib/openssh/sftp-server", "/usr/lib/ssh/sftp-server"]

        options = ["-o", "nonempty",
                   "-o", "sftp_server=sudo " ++ head serverPaths,
                   "-o", "StrictHostKeyChecking=no"]

    home <- liftIO getHomeDirectory

    let keyPath = home </> ".ssh/serverman"
        pubPath = keyPath <.> "pub"

    -- check if a connection to SSH server using public key is possible
    result <- do
      exists <- liftIO $ doesPathExist path
      content <- if exists then liftIO $ listDirectory path else return []

      if not exists || null content then do
        liftIO $ createDirectoryIfMissing True path

        verbose $ "mounting SSHFs: " ++ path

        result <- execute "sshfs" (p ++ noPassword ++ uid ++ options ++ ["-o", "IdentityFile=" ++ keyPath, smConnection ++ ":/", path]) "" False

        state@AppState { temps } <- get
        put $ state { temps = path:temps }

        return result
      else do
        verbose $ "SSHFs already mounted on " ++ path ++ ", continuing"
        return $ Right "already mounted"

    done

    case result of
      Right _ -> runAction servermanAddr keyPath

      Left e -> do
        info $ "it seems to be the first time you are using serverman for configuring " ++ show addr
        write "remotely. serverman will create a user, and add it to sudoers file. an ssh key will be created"
        write "and that will be used for connecting to the server from now on"
        write "you will not be prompted for a password to connect to server with"
        write "please enable password authentication temporarily on your server for this step"

        write $ "Enter password for " ++ connection

        home <- liftIO getHomeDirectory
        password <- liftIO getPassword

        done <- progressText $ "setting up serverman user in server " ++ show addr

        execIfMissing keyPath $ void $ execute "ssh-keygen" ["-N", "", "-f", keyPath] "" True

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

        done

        runAction servermanAddr keyPath

    return $ Left ("could not run action remotely: " ++ show addr) 

    where
      runAction servermanAddr keyPath = do
        state <- get
        r <- liftIO $ threadDelay actionDelay

        put $ state { remoteMode = Just (servermanAddr, keyPath) }
        getOS
        action

        return (Right r)

      noPassword = ["-o", "PasswordAuthentication=no", "-o", "PubkeyAuthentication=yes"]

      modPath path c
        | c == ':' = ":" ++ path
        | otherwise = [c]
        
