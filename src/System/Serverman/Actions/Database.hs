module System.Serverman.Actions.Database (DatabaseParams(..)) where
  import System.Serverman.Utils
  import System.Serverman.Services

  import Control.Monad.Free

  data DatabaseParams = DatabaseParams { database        :: String
                                       , databaseService :: Service
                                       } deriving (Eq)
