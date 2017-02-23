{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.MongoDB (mongodb) where
  import System.Serverman.Actions.Database
  import System.Serverman.Utils hiding (execute)
  import Database.MongoDB
  import qualified Data.ByteString.Char8 as BS
  import Data.List hiding (delete)
  import qualified Data.Text as T
  import Control.Monad
  import System.IO.Error

  mongodb :: DatabaseParams -> IO ()
  mongodb (DatabaseParams { database, dummyData, databaseHost }) = do
    result <- tryIOError $ connect (readHostPort databaseHost)

    case result of
      Right pipe -> do
        e <- access pipe master (T.pack database) run

        close pipe
      Left err -> do
        putStrLn $ show err
        putStrLn $ "[Error] could not connect to MongoDB server " ++ databaseHost
  
    where
      run = do
        when dummyData $ do
          clearCollection
          insertToCollection
          return ()

      clearCollection = delete (select [] (T.pack collectionName))
        where (collectionName, _, _) = dummy

      insertToCollection = insertMany (T.pack collectionName) records
        where
          (collectionName, definitions, rows) = dummy
          records = map (\row -> zipWith (\def value -> def =: row) (map T.pack definitions) row) rows
        

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
    
