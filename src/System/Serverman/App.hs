module System.Serverman.App ( AppState (..)
                            , App
                            , runApp) where

  import qualified System.Serverman.Services (Repository)

  data AppState rep = AppState { remoteMode :: Maybe (Address, String)
                                , repository :: Repository
                                } deriving (Show)

  instance Default AppState where
    def = AppState { remoteMode = Nothing
                   , repository = [] }
  type App = StateT AppState IO

  runApp :: App a -> IO (a, AppState)
  runApp k = runStateT k def

