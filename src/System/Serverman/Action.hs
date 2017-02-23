{-# LANGUAGE ScopedTypeVariables #-}

module System.Serverman.Action ( ActionF(..)
                               , Action
                               , newServer
                               , newDatabase
                               , newFileSharing
                               , start
                               , install
                               , detectOS) where

  import System.Serverman.Actions.WebServer
  import System.Serverman.Actions.FileSharing
  import System.Serverman.Actions.Database
  import System.Serverman.Actions.Env
  import System.Serverman.Utils
  import System.Serverman.Services

  import System.Directory
  import System.FilePath
  import System.IO
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Free
  import System.IO.Error
  import Data.Char

  data ActionF x = NewWebServer ServerParams x
                 | NewDatabase DatabaseParams x
                 | NewFileSharing FileSharingParams x
                 | DetectOS (OS -> x)
                 | Install Service OS x
                 | Start Service OS x

  instance Functor ActionF where
    fmap f (NewWebServer params x) = NewWebServer params (f x)
    fmap f (NewDatabase params x) = NewDatabase params (f x)
    fmap f (NewSharedFolder params x) = NewSharedFolder params (f x)
    fmap f (Install service os x) = Install service os (f x)
    fmap f (Start service os x) = Start service os (f x)
    fmap f (DetectOS x) = DetectOS (f . x)

  type Action = Free ActionF

  newServer :: ServerParams -> Action ()
  newServer params = liftF $ NewWebServer params ()

  newDatabase :: DatabaseParams -> Action ()
  newDatabase params = liftF $ NewDatabase params ()

  newFileSharing :: FileSharingParams -> Action ()
  newFileSharing params = liftF $ NewFileSharing params ()

  install :: Service -> OS -> Action ()
  install service os = liftF $ Install service os ()

  start :: Service -> OS -> Action ()
  start service os = liftF $ Start service os ()

  detectOS :: Action OS
  detectOS = liftF $ DetectOS id
