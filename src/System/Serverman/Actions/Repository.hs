{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Repository (fetchRepo, findService) where
  import System.Serverman.Utils
  import System.Directory
  import System.Serverman.Services hiding (info)
  import System.Serverman.Actions.Env
  import System.Serverman.Log
  import System.Serverman.Types

  import System.FilePath
  import Data.Maybe
  import Data.Either
  import Data.Aeson
  import Data.Aeson.Types
  import GHC.Generics
  import qualified Data.Map as M
  import Control.Monad.State hiding (liftIO)
  import qualified Data.ByteString.Lazy.Char8 as BS
  import qualified Data.Text as T
  import Data.List

  sourceURL = "https://github.com/mdibaiee/serverman"

  findService :: String -> App (Maybe Service)
  findService n = do
    (AppState { repository }) <- get
    verbose $ "searching for service " ++ n
    return $ find (\a -> name a == n) repository

  fetchRepo :: Bool -> App Repository
  fetchRepo update = do
    verbose "fetching repository"

    state@(AppState { repositoryURL }) <- get
    dir <- liftIO $ getAppUserDataDirectory "serverman"
    let path = dir </> "repository"
    let source = dir </> "source"

    execIfMissing path $ do
      verbose "repository missing... cloning repository"
      info $ "cloning " ++ repositoryURL ++ " in " ++ path
      execute "git" ["clone", repositoryURL, path] "" True
      return ()

    execIfMissing source $ do
      verbose "serverman source missing... cloning repository"

      info $ "cloning " ++ sourceURL ++ " in " ++ source
      execute "git" ["clone", sourceURL, source] "" True
      return ()

    when update $ do
      verbose "updating repository"

      exec "git" ["pull", "origin", "master"] "" (Just path) True
      exec "git" ["pull", "origin", "master"] "" (Just source) True
      return ()

    content <- liftIO $ readFile (path </> "repository.json")

    let json = decode (BS.pack content) :: Maybe [Object]

    case json of
      Just d -> do
        let repo :: Maybe [Either String Service] = mapM toService d

        case repo of
          Just list -> do
            let r = rights list

            state <- get
            put $ state { repository = r }
            return $ rights list

          Nothing -> do
            err $ "parsing repository data failed, please try re-fetching the repository."
            return []
      Nothing -> do
        err $ "parsing repository data failed, please try re-fetching the repository."
        return []

    where
      toService obj = do
        return $
          flip parseEither obj $ \object -> do
            name <- object .: "name"
            version <- object .: "version"
            service <- object .: "service"
            category <- object .: "category"
            packages <- object .: "packages"
            dependencies <- object .: "dependencies"

            pkglist :: [(OS, [String])] <- map (\(os, name) -> (read os, name)) <$> M.toList <$> parseJSON packages

            return Service { name = name
                           , version = version
                           , service = service
                           , category = category
                           , packages = pkglist
                           , dependencies = dependencies
                           }
