{-# LANGUAGE NamedFieldPuns #-}
module System.Serverman.Actions.MySQL (mysql) where
  import System.Serverman.Actions.Database
  import System.Serverman.Utils

  mysql :: DatabaseParams -> IO ()
  mysql (DatabaseParams { database, databaseService }) = do
    return ()
