{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term.FileSharing (mode, handle, Params(..)) where
  import System.Console.CmdArgs
  import qualified System.Serverman as S
  import qualified System.Term.Remote as R
  import Control.Monad
  import System.Exit
  import System.Directory hiding (writable)

  data Params = Params { directory      :: String
                       , user           :: String
                       , pass           :: String
                       , port           :: String
                       , writable       :: Bool
                       , anonymous      :: Bool
                       , anonymousWrite :: Bool
                       , recreateUser   :: Bool
                       , service        :: String
                       , remote         :: FilePath
                       } deriving (Show, Data, Typeable)

  mode = Params { directory      = "/srv/ftp/" &= typDir &= help "directory to share, defaults to /srv/ftp/" &= explicit &= name "directory"
                , user           = "serverman" &= typDir &= help "username, defaults to serverman" &= explicit &= name "user"
                , pass           = "" &= help "password, defaults to serverman (please change this to avoid security risks)" &= explicit &= name "password"
                , anonymous      = False &= help "allow anonymous connections, defaults to False" &= explicit &= name "anonymous"
                , anonymousWrite = False &= help "allow anonymous write operations, defaults to False" &= explicit &= name "anonymous-write"
                , writable       = True &= help "allow write operations, defaults to True" &= explicit &= name "writable"
                , port           = "21" &= help "service port, defaults to 21" &= explicit &= name "port"
                , service        = "vsftpd" &= help "service to use for file sharing, defaults to vsftpd" &= explicit &= name "service"
                , recreateUser   = False &= help "recreate the user" &= explicit &= name "recreate-user"
                , remote    = def &= help "file to read remote hosts from. each line should contain a host:port" &= typDir &= explicit &= name "remote"
                } &= explicit &= name "filesharing"

  handle (Params { directory, user, pass, port, anonymous, anonymousWrite, writable, service, recreateUser, remote }) =
    R.handle remote $ do
      let serviceName = read service

      let params = S.FileSharingParams { S.fDirectory      = directory
                                       , S.fUser           = user
                                       , S.fPass           = pass
                                       , S.fPort           = port
                                       , S.fAnonymous      = anonymous
                                       , S.fAnonymousWrite = anonymousWrite
                                       , S.fWritable       = writable
                                       , S.fService        = serviceName
                                       , S.fRecreateUser   = recreateUser
                                       }

      return $ S.detectOS >>= (S.install serviceName)
             >> S.detectOS >>= (S.start serviceName)
             >> S.newFileSharing params
