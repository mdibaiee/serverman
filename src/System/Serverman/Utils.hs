module System.Serverman.Utils ( keyvalue
                              , block
                              , nginxSSL
                              , indent
                              , writeFileIfMissing
                              , commandError
                              , appendAfter
                              , execute) where

  import System.IO
  import Control.Monad
  import System.Directory
  import System.Process
  import System.IO.Error
  import Control.Concurrent.Async
  import Data.List
  import Control.Exception

  keyvalue :: [(String, String)] -> String
  keyvalue ((a, b):xs) = a ++ " " ++ b ++ ";\n" ++ keyvalue xs
  keyvalue [] = ""

  block :: String -> String -> String
  block blockName content = blockName ++ " {\n" ++ indent content ++ "}"

  writeFileIfMissing :: FilePath -> String -> IO ()
  writeFileIfMissing path content = do
    exists <- doesFileExist path
    
    when (not exists) $ do
      writeFile path content

  appendAfter :: String -> String -> String -> String
  appendAfter content after line =
    let ls = lines content
        appended = concat $ map (\x -> if x == after then [x, line] else [x]) ls

    in unlines appended

  indent :: String -> String
  indent s = unlines $ map ("\t" ++) (lines s)

  commandError :: String -> String
  commandError command = "[Error] an error occured while running: " ++ command ++ "\nplease try running the command manually."

  execute :: String -> [String] -> String -> Bool -> IO (Either String String)
  execute cmd args stdin logErrors = do
    let command = cmd ++ " " ++ intercalate " " args

    process <- async $ do
      result <- tryIOError $ readProcessWithExitCode cmd args stdin

      case result of
         Right (_, stdout, _) -> return $ Right stdout
         Left err -> do
           when logErrors $ putStrLn (commandError command)
           return $ Left (show err)

    wait process

  nginxSSL = "# from https://cipherli.st/\n\
\# and https://raymii.org/s/tutorials/Strong_SSL_Security_On_nginx.html\n\
\\n\
\ssl_protocols TLSv1 TLSv1.1 TLSv1.2;\n\
\ssl_prefer_server_ciphers on;\n\
\ssl_ciphers 'EECDH+AESGCM:EDH+AESGCM:AES256+EECDH:AES256+EDH';\n\
\ssl_ecdh_curve secp384r1;\n\
\ssl_session_cache shared:SSL:10m;\n\
\ssl_session_tickets off;\n\
\ssl_stapling on;\n\
\ssl_stapling_verify on;\n\
\resolver 8.8.8.8 8.8.4.4 valid=300s;\n\
\resolver_timeout 5s;\n\
\# Disable preloading HSTS for now.  You can use the commented out header line that includes\n\
\# the 'preload' directive if you understand the implications.\n\
\#add_header Strict-Transport-Security 'max-age=63072000; includeSubdomains; preload';\n\
\add_header Strict-Transport-Security 'max-age=63072000; includeSubdomains';\n\
\add_header X-Frame-Options DENY;\n\
\add_header X-Content-Type-Options nosniff;\n\
\\n\
\ssl_dhparam /etc/ssl/certs/dhparam.pem;\n"
