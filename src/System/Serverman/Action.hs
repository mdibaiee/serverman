{-# LANGUAGE ScopedTypeVariables #-}

module System.Serverman.Action ( ActionF(..)
                               , Action
                               , call
                               , fetchRepository
                               , start
                               , stop
                               , install
                               , remote
                               , detectOS) where

  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Repository
  import System.Serverman.Actions.Remote

  import System.Serverman.Types
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

  data ActionF x = Call Service (Maybe FilePath) x
                 | DetectOS x
                 | Install Service x
                 | Remote [Address] (Action ()) x
                 | FetchRepository x
                 | Start Service x
                 | Stop Service x

  instance Functor ActionF where
    fmap f (Call service remote x) = Call service remote (f x)
    fmap f (Install service x) = Install service (f x)
    fmap f (Start service x) = Start service (f x)
    fmap f (Stop service x) = Stop service (f x)
    fmap f (DetectOS x) = DetectOS (f x)
    fmap f (Remote addr action x) = Remote addr action (f x)
    fmap f (FetchRepository x) = FetchRepository (f x)

  type Action = Free ActionF

  call :: Service -> Maybe FilePath -> Action ()
  call service remote = liftF $ Call service remote ()

  install :: Service -> Action ()
  install service = liftF $ Install service ()

  start :: Service -> Action ()
  start service = liftF $ Start service ()

  stop :: Service -> Action ()
  stop service = liftF $ Stop service ()

  detectOS :: Action ()
  detectOS = liftF $ DetectOS ()

  remote :: [Address] -> Action () -> Action ()
  remote addrs action = liftF $ Remote addrs action ()

  fetchRepository :: Action ()
  fetchRepository = liftF $ FetchRepository ()
