module System.Serverman.Services ( Service(..)
                                 , configDirectory) where

  data Service = NGINX
               | MySQL
               | MongoDB
               | VsFTPd
               | LetsEncrypt
                 deriving (Eq, Show)

  class Configurable a where
    configDirectory :: a -> FilePath

  instance Configurable Service where
    configDirectory NGINX = "/etc/nginx/"
    configDirectory MySQL = "/etc/mysql/"
    configDirectory MongoDB = "/etc/mongodb"
    configDirectory VsFTPd = "/etc/vsftpd.conf"

  instance Read Service where
    readsPrec _ service
          | service == "nginx" = [(NGINX, [])]
          | service == "mysql" = [(MySQL, [])]
          | service == "mongodb" = [(MongoDB, [])]
          | service == "vsftpd" = [(VsFTPd, [])]
          | service == "letsencrypt" = [(LetsEncrypt, [])]
