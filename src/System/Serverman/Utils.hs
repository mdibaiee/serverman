module System.Serverman.Utils ( keyvalue
                              , nginxBlock
                              , nginxSSL
                              , writeFileIfMissing
                              , commandError) where

  import System.IO
  import Control.Monad
  import System.Directory

  keyvalue :: [(String, String)] -> String
  keyvalue ((a, b):xs) = a ++ " " ++ b ++ ";\n" ++ keyvalue xs
  keyvalue [] = ""

  nginxBlock :: String -> String -> String
  nginxBlock blockName content = blockName ++ " {\n" ++ indent content ++ "}"

  writeFileIfMissing :: FilePath -> String -> IO ()
  writeFileIfMissing path content = do
    exists <- doesFileExist path
    
    when (not exists) $ do
      writeFile path content

  indent :: String -> String
  indent s = unlines $ map ("\t" ++) (lines s)

  commandError :: String -> String
  commandError command = "[Error] an error occured while running: " ++ command ++ "\nplease try running the command manually."

  nginxSSL = "ssl_protocols TLSv1 TLSv1.1 TLSv1.2;\n\
\ssl_prefer_server_ciphers on;\n\
\ssl_dhparam /etc/ssl/certs/dhparam.pem;\n\
\ssl_ciphers 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA';\n\
\ssl_session_timeout 1d;\n\
\ssl_session_cache shared:SSL:50m;\n\
\ssl_stapling on;\n\
\ssl_stapling_verify on;\n\
\add_header Strict-Transport-Security max-age=15768000;"
