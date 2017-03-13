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

  import System.Serverman.Utils
  import System.Serverman.Types
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

  data ActionF x = Call Service Params x
                 | DetectOS (OS -> x)
                 | Install Service OS x
                 | Remote [Address] (Action ()) x
                 | FetchRepository x
                 | Start Service OS x
                 | Stop Service OS x

  instance Functor ActionF where
    fmap f (Call service params x) = Call service params (f x)
    fmap f (Install service os x) = Install service os (f x)
    fmap f (Start service os x) = Start service os (f x)
    fmap f (Stop service os x) = Stop service os (f x)
    fmap f (DetectOS x) = DetectOS (f . x)
    fmap f (Remote addr action x) = Remote addr action (f x)
    fmap f (FetchRepository x) = FetchRepository (f x)

  type Action = Free ActionF

  call :: Service -> Params -> Action ()
  call service params = liftF $ Call service params ()

  install :: Service -> OS -> Action ()
  install service os = liftF $ Install service os ()

  start :: Service -> OS -> Action ()
  start service os = liftF $ Start service os ()

  stop :: Service -> OS -> Action ()
  stop service os = liftF $ Stop service os ()

  detectOS :: Action OS
  detectOS = liftF $ DetectOS id

  remote :: [Address] -> Action () -> Action ()
  remote addrs action = liftF $ Remote addrs action ()

  fetchRepository :: Action ()
  fetchRepository = liftF $ FetchRepository ()
