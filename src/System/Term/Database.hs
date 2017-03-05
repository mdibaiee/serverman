{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term.Database (mode, handle, Params(..)) where
  import System.Console.CmdArgs hiding (name)
  import qualified System.Console.CmdArgs as C (name) 
  import qualified System.Serverman as S
  import qualified System.Term.Remote as R
  import Control.Monad
  import System.Exit
  import System.Directory

  data Params = Params { name         :: String
                       , service      :: String
                       , dummyData    :: Bool
                       , user         :: String
                       , pass         :: String
                       , host         :: String
                       , remote       :: FilePath
                       } deriving (Show, Data, Typeable)

  mode = Params { name      = "test" &= help "database name, defaults to test"
                , service   = "mysql" &= help "service to setup: mysql, defaults to mysql"
                , dummyData = False &= help "generate dummy data in the database" &= explicit &= C.name "dummy-data"
                , user      = "root" &= help "database's username, defaults to root"
                , pass      = "" &= help "database's password, defaults to blank string"
                , host      = "127.0.0.1" &= help "database's host, defaults to localhost"
                , remote    = def &= help "file to read remote hosts from. each line should contain a host:port" &= typDir
                } &= explicit &= C.name "database"

  handle (Params { name, service, dummyData, user, pass, host, remote }) =
    R.handle remote $ do
      let serviceName = read service

      let params = S.DatabaseParams { S.database        = name
                                    , S.databaseService = serviceName
                                    , S.dummyData       = dummyData
                                    , S.databaseUser    = user
                                    , S.databasePass    = pass
                                    , S.databaseHost    = host
                                    }

      return $ S.detectOS >>= (S.install serviceName)
             >> S.detectOS >>= (S.start serviceName)
             >> S.newDatabase params
