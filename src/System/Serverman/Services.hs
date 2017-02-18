module System.Serverman.Services ( Service(..)
                                 , ) where

  data Service = NGINX | Apache deriving (Eq, Show)

  instance Read Service where
    readsPrec _ service
          | service == "nginx" || service == "n" = [(NGINX, [])]
          | service == "apache" || service == "a" = [(Apache, [])]
