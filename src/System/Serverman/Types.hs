{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Serverman.Types ( Service (..)
                              , Repository
                              , AppState (..)
                              , OS (..)
                              , App
                              , Address (..)
                              , Params
                              , runApp
                              , liftedAsync) where
  import Data.Default.Class
  import GHC.Generics
  import Control.Monad.State
  import Control.Concurrent.Async
  import Control.Monad.Trans.Control
  import System.Process

  type Host = String
  type Port = String
  type User = String
  data Address = Address Host Port User 

  type Params = [(String, String)]

  instance Read Address where
    readsPrec _ addr
      | '@' `elem` addr =
            let (user, rest) = (takeWhile (/= '@') addr, tail $ dropWhile (/= '@') addr)
                (host, port) = readHostPort rest "22"
            in [(Address host port user, [])]
      | otherwise = 
            let (host, port) = readHostPort addr "22"
            in [(Address host port "", [])]

      where
        readHostPort str defaultPort
          | ':' `elem` str = (takeWhile (/= ':') str, tail $ dropWhile (/= ':') str)
          | otherwise = (str, defaultPort)

  instance Show Address where
    show (Address host port user)
      | (not . null) user = user ++ "@" ++ show (Address host port "")
      | (not . null) port = show (Address host "" "") ++ ":" ++ port
      | otherwise = host


  data OS = Debian | Arch | Unknown deriving (Eq)

  instance Read OS where
    readsPrec _ os
      | os == "debian" = [(Debian, [])]
      | os == "arch" = [(Arch, [])]
      | os == "_" = [(Unknown, [])]

  instance Show OS where
    show os
      | os == Debian = "debian"
      | os == Arch = "arch"
      | os == Unknown = "_"

  data Service = Service { name         :: String
                         , packages     :: [(OS, [String])]
                         , service      :: String
                         , version      :: String
                         , dependencies :: [String]
                         , category     :: String
                         } deriving (Eq, Generic)

  instance Read Service where
    readsPrec _ service = [(Service { name = service }, [])]

  instance Show Service where
    show Service { name, version } =
      name ++ "@" ++ version

  type Repository = [Service]

  type SourcePort = String
  type DestinationPort = String
  data AppState = AppState { remoteMode    :: Maybe (Address, String)
                           , repository    :: Repository
                           , repositoryURL :: String
                           , os            :: OS
                           , arguments     :: [(String, Maybe String)]
                           , helpArg       :: Bool
                           , verboseMode   :: Bool
                           , ports         :: [(SourcePort, DestinationPort)]
                           , processes     :: [ProcessHandle]
                           , temps         :: [FilePath]
                           }

  instance Show AppState where
    show AppState { remoteMode, repository, repositoryURL, os, arguments, ports, processes, temps, verboseMode } = 
      "remote: " ++ show remoteMode ++ "\n" ++
      "repository:\n" ++
      "  - url: " ++ show repositoryURL ++ "\n" ++
      "  - packages: " ++ show repository ++ "\n" ++
      "operating system: " ++ show os ++ "\n" ++
      "arguments: " ++ show arguments ++ "\n" ++
      "port forwarding: " ++ show ports ++ "\n" ++
      "verbose: " ++ show verboseMode ++ "\n" ++
      "processes: " ++ show (length processes) ++
      "temps: " ++ show (length temps)

  instance Default AppState where
    def = AppState { remoteMode    = Nothing
                   , repository    = def
                   , repositoryURL = "https://github.com/mdibaiee/serverman-repository"
                   , os            = Unknown
                   , arguments     = []
                   , helpArg       = False
                   , verboseMode   = False
                   , ports         = []
                   , processes     = []
                   , temps         = []
                   }
  type App = StateT AppState IO

  runApp :: App a -> IO (a, AppState)
  runApp k = runStateT k def

  liftedAsync :: MonadBaseControl IO m => m a -> m (Async (StM m a))
  liftedAsync m = liftBaseWith $ \runInIO -> async (runInIO m)

