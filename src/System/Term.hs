{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Term ( initialize ) where
  import qualified System.Serverman as S

  import System.Environment
  import System.Directory
  import System.Exit
  import Data.Monoid
  import Data.Maybe
  import Control.Monad
  import Control.Monad.State
  import Data.Default.Class
  import System.FilePath
  import Data.List

  import System.Serverman.Utils hiding (liftIO)
  import System.Serverman.Actions.Repository

  initialize = do
    args <- getArgs

    dir <- liftIO $ getAppUserDataDirectory "serverman"
    let path = dir </> "repository"

    let params = parseParams args
    liftIO $ print params

    -- Fetch repository first
    S.runApp $ do
      S.run (S.fetchRepository)
      S.run (S.detectOS)

      state@(S.AppState { S.repository }) <- get
      put $ state { arguments = rest params }

      case params of
        (Params { listServices = True }) -> liftIO $ do
          mapM_ print repository
        p@(Params { install = Just service }) -> do
          ms <- findService service
          case ms of
            Just s -> handleRemote p $ S.install s
            Nothing -> liftIO $ putStrLn $ "service not found: " ++ service
        p@(Params { rest = (x:xs), remote }) -> do
          case x of
            (service, Nothing) -> do
              ms <- findService service
              case ms of
                Just s -> do
                  handleRemote p $ S.install s
                  S.run $ S.call s remote

                Nothing -> liftIO $ putStrLn $ "could not find any service matching " ++ service
            _ -> liftIO $ putStrLn $ "could not understand your input"

      {-S.run (S.call (head repository) [])-}

    return ()

    where
      handleRemote (Params { remote = Just file }) action = do
        list <- liftIO $ map read . lines <$> readFile file
        S.run (S.remote list action)
      handleRemote (Params { remote = Nothing }) action = S.run action


  data Manage = Start | Stop deriving (Eq, Show)
  data Params = Params { listServices :: Bool
                       , install      :: Maybe String
                       , manage       :: Maybe (Manage, String)
                       , update       :: Bool
                       , remote       :: Maybe FilePath
                       , help         :: Bool
                       , rest         :: [(String, Maybe String)]
                       } deriving (Show)

  instance Default Params where
    def = Params { listServices = False
                 , install      = Nothing
                 , manage       = Nothing
                 , remote       = Nothing
                 , update       = False
                 , help         = False
                 , rest         = []
                 }

  parseParams :: [String] -> Params
  parseParams ("repository":"list":xs) = (parseParams xs) { listServices = True }
  parseParams ("repository":"update":xs) = (parseParams xs) { update = True }
  parseParams ("service":"start":s:xs) = (parseParams xs) { manage = Just (Start, s) }
  parseParams ("service":"stop":s:xs) = (parseParams xs) { manage = Just (Stop, s) }
  parseParams ("install":s:xs) = (parseParams xs) { install = Just s }
  parseParams ("--remote":s:xs) = (parseParams xs) { remote = Just s }
  parseParams ("--help":xs) = (parseParams xs) { help = True }
  parseParams ("-h":xs) = (parseParams xs) { help = True }
  parseParams [] = def { help = True }
  parseParams x = def { rest = toPairs x }
    where
      toPairs [] = []
      toPairs [x] = [(getWord x, Nothing)]
      toPairs (x:y:xs)
        | flagName x && value y = [(getWord x, Just y)] ++ toPairs xs
        | flagName y && value x = [(getWord x, Nothing)] ++ toPairs (y:xs)
        | flagName x && flagName y = [(getWord x, Nothing)] ++ toPairs (y:xs)
        | otherwise = toPairs xs

      flagName = isPrefixOf "-"
      value = not . flagName
      getWord = dropWhile (== '-')
