module System.Serverman.Actions.WebServer (ServerParams(..), ServerType(..)) where
  import System.Serverman.Utils
  import System.Serverman.Services

  import Control.Monad.Free

  data ServerType = Static | PortForwarding deriving (Show, Eq)
  data ServerParams = ServerParams { directory     :: String
                                   , domain        :: String
                                   , port          :: String
                                   , forward       :: String
                                   , ssl           :: Bool
                                   , serverType    :: ServerType
                                   , serverService :: Service
                                   } deriving (Eq)
  instance Show ServerParams where
    show conf 
      | serverService conf == NGINX = 
          let https
                | ssl conf = [ ("ssl_certificate", "/etc/letsencrypt/live/" ++ domain conf ++ "/fullchain.pem")
                             , ("ssl_certificate_key", "/etc/letsencrypt/live/" ++ domain conf ++ "/privkey.pem")
                             , ("include", "ssl.conf")]
                | otherwise = []

              base = [ ("server_name", domain conf)
                     , ("listen", port conf)
                     , ("index", "index.html index.html index.php")
                     ] ++ https
          in 
            case serverType conf of
              Static -> 
                block "server" $ keyvalue (base ++ [("root", directory conf)])

              PortForwarding -> 
                let proxyBlock = block "location /" $
                                    keyvalue ([ ("proxy_pass", "http://127.0.0.1:" ++ forward conf)
                                              , ("proxy_set_header", "X-Forwarded-Host $host")
                                              , ("proxy_set_header", "X-Forwarded-Server $host")
                                              , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
                                              ])
                in block "server" $ keyvalue base ++ proxyBlock

      | otherwise = "Unknown service provider"
