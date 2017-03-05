{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term.WebServer (mode, handle, Params(..)) where
  import System.Console.CmdArgs
  import qualified System.Serverman as S
  import qualified System.Term.Remote as R
  import Control.Monad
  import System.Exit
  import System.Directory

  data Params = Params { directory :: String
                       , domain    :: String
                       , port      :: String
                       , forward   :: String
                       , service   :: String
                       , ssl       :: Bool
                       , email     :: String
                       , remote    :: FilePath
                       } deriving (Show, Data, Typeable)

  mode = Params { directory = "/var/www/html/" &= typDir &= help "directory to serve static files from, defaults to /var/www/html/" 
                , domain    = "test.dev" &= typ "DOMAIN" &= help "domain/server name, defaults to test.dev"
                , port      = def &= typ "PORT" &= help "port number to listen to, defaults to 80 for http and 443 for https"
                , forward   = def &= typ "PORT" &= help "the port to forward to (in case of a port-forwarding server)"
                , ssl       = False &= help "create a letsencrypt certificate for this domain, defaults to false"
                , email     = def &= help "email required for registering your certificate"
                , service   = "nginx" &= help "service to build config for: nginx, defaults to nginx"
                , remote    = def &= help "file to read remote hosts from. each line should contain a host:port" &= typDir &= explicit &= name "remote"
                } &= explicit &= name "webserver"

  handle (Params { directory, domain, port, ssl, forward, service, email, remote }) =
    R.handle remote $ do
      when (ssl && null email) $ die "Email is required for generating a certificate"

      let serverType 
            | (not . null) forward = S.PortForwarding
            | otherwise = S.Static

      let serviceName = read service :: S.Service

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
      return $ S.detectOS >>= (S.install serviceName)
             >> S.detectOS >>= (S.start serviceName)
             >> S.newServer params
