{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term ( initialize ) where
  import System.Serverman.Services
  import qualified System.Serverman as S

  import System.Console.CmdArgs
  import qualified System.Console.CmdArgs.Explicit as E
  import System.Environment
  import Data.Monoid
  import Data.Maybe

  initialize = do
    args <- getArgs
    let mode = cmdArgsMode $ modes [install, webserver, database] 
                           &= program "serverman"
                           &= summary "serverman v0.1.0, (C) Mahdi Dibaiee 2017"
                           &= helpArg [name "h"]

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
            p@(WebServerParams {}) -> webserverSetup p
            p@(InstallParams {})   -> manualInstall p
            p@(DatabaseParams {})  -> databaseSetup p
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
                                }
              | DatabaseParams { databaseName :: String
                               , dService     :: String }

              | InstallParams { iService :: String }

              deriving (Show, Data, Typeable)

  webserver = WebServerParams { directory = "/var/www/html/" &= typDir &= help "directory to serve static files from, defaults to /var/www/html/" 
                              , domain    = "test.dev" &= typ "DOMAIN" &= help "domain/server name, defaults to test.dev"
                              , port      = def &= typ "PORT" &= help "port number to listen to, defaults to 80 for http and 443 for https"
                              , forward   = def &= typ "PORT" &= help "the port to forward to (in case of a port-forwarding server)"
                              , ssl       = False &= help "create a letsencrypt certificate for this domain, defaults to false"
                              , wService  = "nginx" &= help "service to build config for: nginx, defaults to nginx" &= explicit &= name "service"
                              } &= explicit &= name "webserver"

  database = DatabaseParams { databaseName = "test" &= help "database name, defaults to test" &= explicit &= name "name"
                            , dService     = "mysql" &= help "service to setup: mysql, defaults to mysql" &= explicit &= name "service"
                            } &= explicit &= name "database"


  install = InstallParams { iService = def &= argPos 0
                          } &= explicit &= name "install"

  webserverSetup (WebServerParams { directory, domain, port, ssl, forward, wService }) = do
    let serverType 
          | (not . null) forward = S.PortForwarding
          | otherwise = S.Static

    let serviceName = read wService :: Service

    let portNumber
          | (not . null) port = port
          | ssl = "403"
          | otherwise = "80"

    let params = S.ServerParams { S.directory     = directory
                                , S.domain        = domain
                                , S.port          = portNumber
                                , S.ssl           = ssl
                                , S.forward       = forward
                                , S.serverType    = serverType
                                , S.serverService = serviceName
                                }
    S.run $ S.newServer params

  manualInstall (InstallParams { iService }) = do
    S.run $ S.detectOS >>= (S.install (read iService))

  databaseSetup (DatabaseParams { databaseName, dService }) = do
    let params = S.DatabaseParams { S.database = databaseName
                                  , S.databaseService = read dService }

    S.run $ S.newDatabase params

