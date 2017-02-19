module System.Serverman.Services ( Service(..)
                                 , configDirectory) where

  data Service = NGINX
               | MySQL
                 deriving (Eq, Show)

  class Configurable a where
    configDirectory :: a -> FilePath

  instance Configurable Service where
    configDirectory NGINX = "/etc/nginx/"
    configDirectory mysql = "/etc/mysql/"

  instance Read Service where
    readsPrec _ service
          | service == "nginx" = [(NGINX, [])]
          | service == "mysql" = [(MySQL, [])]
