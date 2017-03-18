{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Types ( Service (..)
                              , Repository
                              , AppState (..)
                              , OS (..)
                              , App
                              , Address (..)
                              , Params
                              , runApp) where
  import Data.Default.Class
  import GHC.Generics
  import Control.Monad.State

  type Host = String
  type Port = String
  type User = String
  data Address = Address Host Port User 

  type Params = [(String, String)]

  instance Read Address where
    readsPrec _ addr
      | '@' `elem` addr =
            let (user, rest) = (takeWhile (/= '@') addr, tail $ dropWhile (/= '@') addr)
                (host, port) = readHostPort rest
            in [(Address host port user, [])]
      | otherwise = 
            let (host, port) = readHostPort addr
            in [(Address host port "", [])]

      where
        readHostPort str = (takeWhile (/= ':') str, tail $ dropWhile (/= ':') str)

  instance Show Address where
    show (Address host port user)
      | (not . null) user = user ++ "@" ++ show (Address host port "")
      | (not . null) port = show (Address host "" "") ++ ":" ++ port
      | otherwise = host


  data OS = Debian | Arch | Mac | Unknown deriving (Eq)

  instance Read OS where
    readsPrec _ os
      | os == "debian" = [(Debian, [])]
      | os == "arch" = [(Arch, [])]
      | os == "mac" = [(Mac, [])]
      | os == "_" = [(Unknown, [])]

  instance Show OS where
    show os
      | os == Debian = "debian"
      | os == Arch = "arch"
      | os == Mac = "mac"
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
    show (Service { name, version }) =
      name ++ "@" ++ version

  type Repository = [Service]

  data AppState = AppState { remoteMode    :: Maybe (Address, String)
                           , repository    :: Repository
                           , repositoryURL :: String
                           , os            :: OS
                           , arguments     :: [(String, Maybe String)]
                           } deriving (Show)

  instance Default AppState where
    def = AppState { remoteMode    = Nothing
                   , repository    = def
                   , repositoryURL = "https://github.com/mdibaiee/serverman-repository"
                   , os            = Unknown
                   , arguments     = []
                   }
  type App = StateT AppState IO

  runApp :: App a -> IO (a, AppState)
  runApp k = runStateT k def

