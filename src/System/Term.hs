{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Term ( initialize ) where
  import qualified System.Serverman as S
  import System.Serverman.Log

  import System.Environment
  import System.Directory
  import Data.Monoid
  import Data.Maybe
  import Control.Monad
  import Control.Monad.State
  import Data.Default.Class
  import System.FilePath
  import Data.List
  import System.Process
  import Control.Concurrent

  import System.Serverman.Utils hiding (liftIO)
  import System.Serverman.Actions.Repository

  initialize = do
    -- read arguments
    args <- getArgs

    dir <- getAppUserDataDirectory "serverman"

    -- parse parameters
    let params = parseParams args
        isHelp = or $ map (`elem` args) ["help", "--help", "-h", "-?"]

    -- Fetch repository first
    S.runApp $ do
      when (verboseM params) $ do
        state <- get
        put $ state { verboseMode = True }
        verbose "verbose mode on"

      verbose $ show params

      -- fetch repository if running for the first time, set state
      S.run (S.fetchRepository)

      -- detect local operating system
      S.run (S.detectOS)

      state@(S.AppState { S.repository }) <- get
      put $ state { arguments = rest params, helpArg = isHelp }

      case params of
        -- list services in repository
        (Params { listServices = True }) -> do
          mapM_ (write . show) repository

        -- install a service
        p@(Params { install = Just service }) -> do
          verbose $ "preparing to install " ++ service
          ms <- findService service
          case ms of
            Just s -> handleRemote p $ S.install s
            Nothing -> die $ "service not found: " ++ service

        p@(Params { update = True }) -> S.run (S.updateRepository)

        p@(Params { manage = Just (act, service) }) -> do
          verbose $ "preparing to " ++ show act ++ " " ++ service
          ms <- findService service
          case ms of
            Just s -> do
              case act of
                Start -> 
                  handleRemote p $ S.start s
                Stop ->
                  handleRemote p $ S.stop s

            Nothing -> 
              die $ "could not find any service matching " ++ service

        -- install and call a service
        p@(Params { rest = (x:xs), remote }) -> do
          case x of
            (service, Nothing) -> do
              verbose $ "preparing to call " ++ service

              ms <- findService service
              case ms of
                Just s -> do
                  when (not isHelp) $ do
                    handleRemote p $ S.install s

                  S.run $ S.call s remote

                Nothing -> do
                  if isHelp then
                    servermanHelp
                  else
                    die $ "could not find any service matching " ++ service
        _ -> servermanHelp

      -- after the program is done, terminate remaining processes
      -- and unmount/remove leftover temporary directories
      state@(S.AppState { S.processes, S.temps }) <- get
      put $ state { remoteMode = Nothing }

      mapM_ (liftIO . terminateProcess) processes
      mapM_ clearTemp temps

    return ()

    where
      clearTemp path = execIfExists path $ do
        execute "fusermount" ["-u", path] "" False
        liftIO $ removeDirectoryRecursive path
      -- if remote mode is set, read the file and run the action
      -- on servers, otherwise run action locally
      handleRemote (Params { remote = Just file }) action = do
        list <- liftIO $ map read . lines <$> readFile file
        S.run (S.remote list action)
      handleRemote (Params { remote = Nothing }) action = S.run action

      servermanHelp = do
        write "serverman [--options] [command/service] [--service-options]"

        write $ mkHelp "commands"
                          [ ("install <service>", "install a service")
                          , ("repository list", "list services")
                          , ("repository update", "update repository")
                          , ("service start <service>", "start the service")
                          , ("service stop <service>", "stop the service")
                          , ("--remote <file>", "run in remote mode: takes a path to a file containing username@ip:port lines")]

        write "to learn about a service's options, run |serverman <service> --help|"


  data Manage = Start | Stop deriving (Eq, Show)
  data Params = Params { listServices :: Bool
                       , install      :: Maybe String
                       , manage       :: Maybe (Manage, String)
                       , update       :: Bool
                       , remote       :: Maybe FilePath
                       , rest         :: [(String, Maybe String)]
                       , verboseM     :: Bool
                       }
  
  instance Show Params where
    show (Params { listServices, install, manage, update, remote, rest, verboseM }) =
      keyvalue [ ("list-services", show listServices)
               , ("install", show install)
               , ("manage", show manage)
               , ("update", show update)
               , ("remote", show remote)
               , ("rest", show rest)
               , ("verbose", show verboseM)] ": "

  instance Default Params where
    def = Params { listServices = False
                 , install      = Nothing
                 , manage       = Nothing
                 , remote       = Nothing
                 , update       = False
                 , rest         = []
                 , verboseM     = False
                 }

  parseParams :: [String] -> Params
  parseParams ("repository":"list":xs) = (parseParams xs) { listServices = True }
  parseParams ("repository":"update":xs) = (parseParams xs) { update = True }
  parseParams ("service":"start":s:xs) = (parseParams xs) { manage = Just (Start, s) }
  parseParams ("service":"stop":s:xs) = (parseParams xs) { manage = Just (Stop, s) }
  parseParams ("install":s:xs) = (parseParams xs) { install = Just s }
  parseParams ("--remote":s:xs) = (parseParams xs) { remote = Just s }
  parseParams ("--verbose":xs) = (parseParams xs) { verboseM = True }
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
