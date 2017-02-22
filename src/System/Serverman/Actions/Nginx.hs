{-# LANGUAGE NamedFieldPuns #-}
module System.Serverman.Actions.Nginx (nginx) where
  import System.Serverman.Action
  import System.Serverman.Actions.WebServer
  import System.Serverman.Utils
  import System.Serverman.Services

  import System.Directory
  import System.IO
  import System.IO.Error
  import System.FilePath
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Free
  import Data.List

  nginx :: ServerParams -> IO ()
  nginx params@(ServerParams { ssl, serverService, domain, directory, serverType, email }) = 
    do
      -- Turn SSL off at first, because we have not yet received a certificate
      let content = show (params { ssl = False, port = "80" })
          mainConfig = configDirectory serverService </> "nginx.conf"
          parent = configDirectory serverService </> "serverman-configs"
          path = parent </> domain
          targetDir = directory

      createDirectoryIfMissing True targetDir
      createDirectoryIfMissing True parent

      writeIncludeStatementIfMissing mainConfig parent

      when ssl $ do
        let sslPath = configDirectory serverService </> "ssl.conf"
        writeFileIfMissing sslPath nginxSSL
        putStrLn $ "wrote ssl configuration to " ++ sslPath

      writeFile path content

      putStrLn $ "wrote your configuration file to " ++ path
      
      wait =<< restart

      when ssl $ do
        let dhparamPath = "/etc/ssl/certs/dhparam.pem"
        dhExists <- doesFileExist dhparamPath

        when (not dhExists) $ do
          dhparam <- async $ execute "openssl" ["dhparam", "-out", dhparamPath, "2048"] "" True
          wait dhparam
          return ()

        case serverType of
          Static -> do
            letsencrypt <- async $ createCert path "letsencrypt"
              
            wait letsencrypt
          _ -> do
            putStrLn $ "you should use letsencrypt to create a certificate for your domain"
            putStrLn $ "and put it in /etc/letsencrypt/live/" ++ domain ++ "/fullchain.pem"
            putStrLn $ "my suggestion is running this command:"
            putStrLn $ "sudo letsencrypt certonly --webroot --webroot-path <YOUR_APPLICATION_DIRECTORY> -d " ++ domain 

        putStrLn $ "for more information, see: https://certbot.eff.org/"
      return ()
    where
      restart = async $ do
        result <- execute "systemctl" ["restart", "nginx"] "" True
        case result of
          Left err -> return ()
          Right _ ->
            putStrLn $ "restarted " ++ show serverService

      createCert path cmd = do
        result <- execute cmd ["certonly", "--webroot", "--webroot-path", directory, "-d", domain, "--email", email, "--agree-tos", "-n"] "" False
        case result of
          Left _ -> if cmd == "letsencrypt" then createCert path "certbot" else return ()
          Right stdout -> do
            putStrLn stdout

            when (not ("error" `isInfixOf` stdout)) $ do
              writeFile path (show params)
              wait =<< restart

      writeIncludeStatementIfMissing path target = do
        content <- readFile path

        let statement = "include " ++ target ++ "/*;"

        when (not (statement `isInfixOf` content)) $ do
          let newContent = appendAfter content "http {" (indent . indent $ statement)

          writeFile path newContent


