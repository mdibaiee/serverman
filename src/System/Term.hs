{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term ( initialize ) where
  import System.Serverman.Services
  import qualified System.Serverman as S

  import System.Console.CmdArgs
  import qualified System.Console.CmdArgs.Explicit as E
  import System.Environment
  import System.Directory
  import System.Exit
  import Data.Monoid
  import Data.Maybe
  import Control.Monad

  initialize = do
    args <- getArgs
    let mode = cmdArgsMode $ modes [install, webserver, database, filesharing] 
                           &= program "serverman"
                           &= summary "serverman v0.1.0, (C) Mahdi Dibaiee 2017"
                           &= helpArg [name "h"]

    user <- getEnv "USER"

    {-when (user == "ROOT") $ do-}
      {-putStrLn $ "It's recommended that you don't run serverman as root."-}
      {-putStrLn $ "Serverman will automatically use sudo whenever needed."-}

    let fixArgs
                  | null args = ["--help"]
                  | otherwise = args

    let result = E.process mode fixArgs

    case result of 
      Right (CmdArgs args help version _ _) -> 
        if isJust help then
          putStrLn $ fromJust help
        else if isJust version then
          putStrLn $ fromJust version
        else
          case args of
            p@(WebServerParams {})   -> webserverSetup p
            p@(InstallParams {})     -> manualInstall p
            p@(DatabaseParams {})    -> databaseSetup p
            p@(FileSharingParams {}) -> fileSharingSetup p
      Left err ->
        print err

    return ()

  -- WEB SERVER 
  data Params = WebServerParams { directory :: String
                                , domain    :: String
                                , port      :: String
                                , forward   :: String
                                , wService  :: String
                                , ssl       :: Bool
                                , email     :: String
                                }
              | DatabaseParams { databaseName :: String
                               , dService     :: String
                               , dummyData    :: Bool
                               , dUser        :: String
                               , dPass        :: String
                               , dHost        :: String
                               }

              | FileSharingParams { fDirectory      :: String
                                  , fUser           :: String
                                  , fPass           :: String
                                  , fPort           :: String
                                  , fWritable       :: Bool
                                  , fAnonymous      :: Bool
                                  , fAnonymousWrite :: Bool
                                  , fRecreateUser   :: Bool
                                  , fService        :: String
                                  }

              | InstallParams { iService :: String }

              deriving (Show, Data, Typeable)

  webserver = WebServerParams { directory = "/var/www/html/" &= typDir &= help "directory to serve static files from, defaults to /var/www/html/" 
                              , domain    = "test.dev" &= typ "DOMAIN" &= help "domain/server name, defaults to test.dev"
                              , port      = def &= typ "PORT" &= help "port number to listen to, defaults to 80 for http and 443 for https"
                              , forward   = def &= typ "PORT" &= help "the port to forward to (in case of a port-forwarding server)"
                              , ssl       = False &= help "create a letsencrypt certificate for this domain, defaults to false"
                              , email     = def &= help "email required for registering your certificate"
                              , wService  = "nginx" &= help "service to build config for: nginx, defaults to nginx" &= explicit &= name "service"
                              } &= explicit &= name "webserver"

  database = DatabaseParams { databaseName = "test" &= help "database name, defaults to test" &= explicit &= name "name"
                            , dService     = "mysql" &= help "service to setup: mysql, defaults to mysql" &= explicit &= name "service"
                            , dummyData    = False &= help "generate dummy data in the database" &= explicit &= name "dummy-data"
                            , dUser        = "root" &= help "database's username, defaults to root" &= explicit &= name "user"
                            , dPass        = "" &= help "database's password, defaults to blank string" &= explicit &= name "password"
                            , dHost        = "127.0.0.1" &= help "database's host, defaults to localhost" &= explicit &= name "host"
                            } &= explicit &= name "database"

  filesharing = FileSharingParams { fDirectory      = "/srv/ftp/" &= typDir &= help "directory to share, defaults to /srv/ftp/" &= explicit &= name "directory"
                                  , fUser           = "serverman" &= typDir &= help "username, defaults to serverman" &= explicit &= name "user"
                                  , fPass           = "" &= help "password, defaults to serverman (please change this to avoid security risks)" &= explicit &= name "password"
                                  , fAnonymous      = False &= help "allow anonymous connections, defaults to False" &= explicit &= name "anonymous"
                                  , fAnonymousWrite = False &= help "allow anonymous write operations, defaults to False" &= explicit &= name "anonymous-write"
                                  , fWritable       = True &= help "allow write operations, defaults to True" &= explicit &= name "writable"
                                  , fPort           = "21" &= help "service port, defaults to 21" &= explicit &= name "port"
                                  , fService        = "vsftpd" &= help "service to use for file sharing, defaults to vsftpd" &= explicit &= name "service"
                                  , fRecreateUser   = False &= help "recreate the user" &= explicit &= name "recreate-user"
                                  } &= explicit &= name "filesharing"


  install = InstallParams { iService = def &= argPos 0
                          } &= explicit &= name "install"

  webserverSetup (WebServerParams { directory, domain, port, ssl, forward, wService, email }) = do
    when (ssl && null email) $ die "Email is required for generating a certificate"

    let serverType 
          | (not . null) forward = S.PortForwarding
          | otherwise = S.Static

    let serviceName = read wService :: Service

    let portNumber
          | (not . null) port = port
          | ssl = "443"
          | otherwise = "80"

    absoluteDirectory <- makeAbsolute directory

    let params = S.ServerParams { S.wDirectory    = absoluteDirectory
                                , S.domain        = domain
                                , S.port          = portNumber
                                , S.ssl           = ssl
                                , S.forward       = forward
                                , S.serverType    = serverType
                                , S.serverService = serviceName
                                , S.email         = email
                                }
    S.run $ S.detectOS >>= (S.install serviceName)
         >> S.detectOS >>= (S.start serviceName)
         >> S.newServer params

  manualInstall (InstallParams { iService }) = do
    let serviceName = read iService :: Service

    S.run $ S.detectOS >>= (S.install serviceName)
         >> S.detectOS >>= (S.start serviceName)
    

  databaseSetup (DatabaseParams { databaseName, dService, dummyData, dUser, dPass, dHost }) = do
    let serviceName = read dService

    let params = S.DatabaseParams { S.database        = databaseName
                                  , S.databaseService = serviceName
                                  , S.dummyData       = dummyData
                                  , S.databaseUser    = dUser
                                  , S.databasePass    = dPass
                                  , S.databaseHost    = dHost
                                  }

    S.run $ S.detectOS >>= (S.install serviceName)
         >> S.detectOS >>= (S.start serviceName)
         >> S.newDatabase params

  fileSharingSetup (FileSharingParams { fDirectory, fUser, fPass, fPort, fAnonymous, fAnonymousWrite, fWritable, fService, fRecreateUser }) = do
    let serviceName = read fService

    let params = S.FileSharingParams { S.fDirectory      = fDirectory
                                     , S.fUser           = fUser
                                     , S.fPass           = fPass
                                     , S.fPort           = fPort
                                     , S.fAnonymous      = fAnonymous
                                     , S.fAnonymousWrite = fAnonymousWrite
                                     , S.fWritable       = fWritable
                                     , S.fService        = serviceName
                                     , S.fRecreateUser   = fRecreateUser
                                     }

    S.run $ S.detectOS >>= (S.install serviceName)
         >> S.detectOS >>= (S.start serviceName)
         >> S.newFileSharing params

