{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.MySQL (mysql) where
  import System.Serverman.Actions.Database
  import System.Serverman.Utils hiding (execute)
  import Database.MySQL.Base
  import qualified Data.ByteString.Char8 as BS
  import Data.List
  import Control.Monad

  mysql :: DatabaseParams -> IO ()
  mysql (DatabaseParams { database, dummyData, databaseUser, databasePass, databaseHost }) = do
    conn <- connect $ defaultConnectInfo { connectUser = databaseUser, connectPassword = databasePass, connectHost = databaseHost }

    query conn $ BS.pack ("CREATE DATABASE IF NOT EXISTS " ++ database)
    

    when dummyData $ do
      let (tableName, _, _) = dummy

      query conn $ BS.pack createDummyTables
      query conn $ BS.pack clearTable
      query conn $ BS.pack insertToDummyTables

      putStrLn $ "Created dummy table '" ++ tableName ++ "' and filled it with data."
      return ()

    return ()

  clearTable = "DELETE FROM " ++ tableName
    where (tableName, _, _) = dummy

  createDummyTables = createTable dummy
    where
      createTable (tableName, columns, rows) = "CREATE TABLE IF NOT EXISTS " ++ tableName ++ "(" ++ intercalate "," (map columnDef columns) ++ ")";
      columnDef "children" = "children INT"
      columnDef "birth_date" = "birth_date DATETIME"
      columnDef "gender" = "gender ENUM('Male', 'Female')"
      columnDef name = name ++ " VARCHAR(255)"

  insertToDummyTables = insertTable dummy
    where
      insertTable (tableName, _, rows) = "INSERT INTO " ++ tableName ++ " VALUES " ++ intercalate "," (map insertRow rows)
      insertRow row = "('" ++ intercalate "','" row ++ "')"
    
