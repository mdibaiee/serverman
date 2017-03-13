{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.WebServer (ServerParams(..), ServerType(..)) where
  import System.Serverman.Utils
  import System.Serverman.Services

  import Control.Monad.Free

  data ServerType = Static | PortForwarding deriving (Show, Eq)
  data ServerParams = ServerParams { wDirectory    :: FilePath
                                   , domain        :: String
                                   , port          :: String
                                   , forward       :: String
                                   , email         :: String
                                   , ssl           :: Bool
                                   , serverType    :: ServerType
                                   , serverService :: Service
                                   } deriving (Eq)
  instance Show ServerParams where
    show (ServerParams { wDirectory, domain, port, forward, email, ssl, serverType, serverService }) 
      | name serverService == "nginx" = 
          let redirect
                | ssl = block "server" $
                              semicolon $
                                keyvalue ([ ("listen", "80")
                                          , ("listen", "[::]:80")
                                          , ("server_name", domain)
                                          , ("rewrite", "^ https://$server_name$request_uri? permanent")
                                          ]) " "
                | otherwise = ""
              https
                | ssl = [ ("ssl_certificate", "/etc/letsencrypt/live/" ++ domain ++ "/fullchain.pem")
                        , ("ssl_certificate_key", "/etc/letsencrypt/live/" ++ domain ++ "/privkey.pem")
                        , ("include", "ssl.conf")]
                | otherwise = []

              listen = port ++ (if ssl then " ssl" else "")

              base = [ ("server_name", domain)
                     , ("listen", listen)
                     , ("listen", "[::]:" ++ listen)
                     , ("index", "index.html index.html index.php")
                     ] ++ https
          in 
            case serverType of
              Static -> 
                (block "server" $ keyvalue (base ++ [("root", wDirectory)]) " ") ++ "\n" ++ redirect

              PortForwarding -> 
                let proxyBlock = block "location /" $
                                    semicolon $
                                      keyvalue ([ ("proxy_pass", "http://127.0.0.1:" ++ forward)
                                                , ("proxy_set_header", "X-Forwarded-Host $host")
                                                , ("proxy_set_header", "X-Forwarded-Server $host")
                                                , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
                                                ]) " "
                in (block "server" $ semicolon (keyvalue base " ") ++ proxyBlock) ++ "\n" ++ redirect

      | otherwise = "Unknown service provider"
