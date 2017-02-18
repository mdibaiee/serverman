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
    let mode = cmdArgsMode $ modes [webserver, install] &= program "serverman" &= summary "serverman v0.1.0, (C) Mahdi Dibaiee 2017"

    (CmdArgs args help version _ _) <- E.processArgs mode

    if isJust help then
      putStrLn $ fromJust help
    else if isJust version then
      putStrLn $ fromJust version
    else
      case args of
        p@(WebServerParams _ _ _ _ _ _ _) -> webServer p
        p@(InstallParams _) -> manualInstall p

    return ()

  -- WEB SERVER 
  data Params = WebServerParams { directory :: String
                                , domain    :: String
                                , port      :: String
                                , ssl       :: Bool
                                , forward   :: String
                                , wService   :: String
                                , output    :: String
                                }

              | InstallParams { iService :: String }

              deriving (Show, Data, Typeable)

  webserver = WebServerParams { directory = "/var/www/html/" &= typDir &= help "directory to serve static files from, defaults to /var/www/html/" 
                              , domain = "test.dev" &= typ "DOMAIN" &= help "domain/server name, defaults to test.dev" 
                              , port = def &= typ "PORT" &= help "port number to listen to, defaults to 80 for http and 443 for https" 
                              , forward = def &= typ "PORT" &= help "the port to forward to (in case of a port-forwarding server)" 
                              , ssl = False &= help "create a letsencrypt certificate for this domain, defaults to false" 
                              , wService = "nginx" &= help "service to build config for: (n)ginx, (a)pache, defaults to nginx" &= explicit &= name "service"
                              , output = def &= help "output directory for the selected service, defaults to /etc/nginx for nginx and /etc/apache2 for apache"
                              } &= explicit &= name "webserver"

  install = InstallParams { iService = def &= argPos 0
                          } &= explicit &= name "install"

  webServer (WebServerParams { directory, domain, port, ssl, forward, wService, output }) = do
    let serverType 
          | (not . null) forward = S.PortForwarding
          | otherwise = S.Static

    let serviceName = read wService :: Service

    let portNumber
          | (not . null) port = port
          | ssl = "403"
          | otherwise = "80"

    let outDir
          | (not . null) output = output
          | serviceName == S.NGINX = "/etc/nginx/"
          | serviceName == S.Apache = "/etc/apache2/"

    let params = S.ServerParams { S.directory  = directory
                                , S.domain     = domain
                                , S.port       = portNumber
                                , S.ssl        = ssl
                                , S.forward    = forward
                                , S.serverType = serverType
                                , S.service    = serviceName
                                , S.output     = outDir
                                }
    S.run $ S.newServer params

  manualInstall (InstallParams { iService }) = do
    S.run $ S.detectOS >>= (S.install (read iService))

