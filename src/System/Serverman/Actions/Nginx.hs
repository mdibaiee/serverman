module System.Serverman.Actions.Nginx (nginx) where
  import System.Serverman.Action
  import System.Serverman.Actions.WebServer
  import System.Serverman.Utils

  import System.Directory
  import System.IO
  import System.IO.Error
  import System.FilePath
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Free

  nginx :: ServerParams -> IO ()
  nginx params = 
    do
      -- Turn SSL off at first, because we have not yet received a certificate
      let content = show (params { ssl = False })
          parent = output params </> "configs"
          path = parent </> domain params
          targetDir = directory params

      createDirectoryIfMissing True targetDir
      createDirectoryIfMissing True parent

      when (ssl params) $ do
        let sslPath = output params </> "ssl.conf"
        writeFileIfMissing sslPath nginxSSL
        putStrLn $ "wrote ssl configuration to " ++ sslPath

      writeFile path content

      putStrLn $ "wrote your configuration file to " ++ path

      restart <- async $ do
        let command = "systemctl restart nginx"
        result <- tryIOError $ callCommand command
        case result of
          Left err -> do
            putStrLn $ commandError command
          Right _ ->
            putStrLn $ "restarted " ++ show (service params)

      wait restart
      
      when (ssl params) $ do
        case serverType params of
          Static -> do
            let command = "certbot certonly --webroot --webroot-path " ++ directory params ++ " -d " ++ domain params
            letsencrypt <- async $ do
              result <- tryIOError $ callCommand command
              case result of
                Left err -> do
                  putStrLn $ commandError command
                Right _ -> do
                  putStrLn $ "created a certificate for " ++ domain params
                  writeFile path (show params)
                  
            wait letsencrypt
          _ -> do
            putStrLn $ "you should use letsencrypt to create a certificate for your domain"
            putStrLn $ "and put it in /etc/letsencrypt/live/" ++ domain params ++ "/fullchain.pem"
            putStrLn $ "my suggestion is running this command:"
            putStrLn $ "sudo certbot certonly --webroot --webroot-path <YOUR_APPLICATION_DIRECTORY> -d " ++ domain params

        putStrLn $ "for more information, see: https://certbot.eff.org/"
      return ()
