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

  import System.Serverman.Utils
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
        (Params { install = Just service }) -> do
          ms <- findService service
          case ms of
            Just s -> S.run (S.install s)
            Nothing -> liftIO $ putStrLn $ "service not found: " ++ service
        (Params { rest = (x:xs) }) -> do
          case x of
            (service, Nothing) -> do
              ms <- findService service
              case ms of
                Just s -> S.run (S.call s)
                Nothing -> liftIO $ putStrLn $ "could not find any service matching " ++ service
            _ -> liftIO $ putStrLn $ "could not understand your input"

      {-S.run (S.call (head repository) [])-}

    return ()

    where


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

   {-WEB SERVER -}
  {-data Params = WebServerParams { directory :: String-}
                                {-, domain    :: String-}
                                {-, port      :: String-}
                                {-, forward   :: String-}
                                {-, wService  :: String-}
                                {-, ssl       :: Bool-}
                                {-, email     :: String-}
                                {-, wRemote   :: String-}
                                {-}-}
              {-| DatabaseParams { databaseName :: String-}
                               {-, dService     :: String-}
                               {-, dummyData    :: Bool-}
                               {-, dUser        :: String-}
                               {-, dPass        :: String-}
                               {-, dHost        :: String-}
                               {-, dRemote      :: String-}
                               {-}-}

              {-| FileSharingParams { fDirectory      :: String-}
                                  {-, fUser           :: String-}
                                  {-, fPass           :: String-}
                                  {-, fPort           :: String-}
                                  {-, fWritable       :: Bool-}
                                  {-, fAnonymous      :: Bool-}
                                  {-, fAnonymousWrite :: Bool-}
                                  {-, fRecreateUser   :: Bool-}
                                  {-, fService        :: String-}
                                  {-, fRemote         :: String-}
                                  {-}-}

              {-| InstallParams { iService :: String, remote :: String }-}

              {-deriving (Show, Data, Typeable)-}

  {-webserver = WebServerParams { directory = "/var/www/html/" &= typDir &= help "directory to serve static files from, defaults to /var/www/html/" -}
                              {-, domain    = "test.dev" &= typ "DOMAIN" &= help "domain/server name, defaults to test.dev"-}
                              {-, port      = def &= typ "PORT" &= help "port number to listen to, defaults to 80 for http and 443 for https"-}
                              {-, forward   = def &= typ "PORT" &= help "the port to forward to (in case of a port-forwarding server)"-}
                              {-, ssl       = False &= help "create a letsencrypt certificate for this domain, defaults to false"-}
                              {-, email     = def &= help "email required for registering your certificate"-}
                              {-, wService  = "nginx" &= help "service to build config for: nginx, defaults to nginx" &= explicit &= name "service"-}
                              {-, wRemote = def &= help "path to the file containing list of remote addresses in the format: user@host:port"-}
                              {-} &= explicit &= name "webserver"-}

  {-database = DatabaseParams { databaseName = "test" &= help "database name, defaults to test" &= explicit &= name "name"-}
                            {-, dService     = "mysql" &= help "service to setup: mysql, defaults to mysql" &= explicit &= name "service"-}
                            {-, dummyData    = False &= help "generate dummy data in the database" &= explicit &= name "dummy-data"-}
                            {-, dUser        = "root" &= help "database's username, defaults to root" &= explicit &= name "user"-}
                            {-, dPass        = "" &= help "database's password, defaults to blank string" &= explicit &= name "password"-}
                            {-, dHost        = "127.0.0.1" &= help "database's host, defaults to localhost" &= explicit &= name "host"-}
                            {-, dRemote = def &= help "path to the file containing list of remote addresses in the format: user@host:port"-}
                            {-} &= explicit &= name "database"-}

  {-filesharing = FileSharingParams { fDirectory      = "/srv/ftp/" &= typDir &= help "directory to share, defaults to /srv/ftp/" &= explicit &= name "directory"-}
                                  {-, fUser           = "serverman" &= typDir &= help "username, defaults to serverman" &= explicit &= name "user"-}
                                  {-, fPass           = "" &= help "password, defaults to serverman (please change this to avoid security risks)" &= explicit &= name "password"-}
                                  {-, fAnonymous      = False &= help "allow anonymous connections, defaults to False" &= explicit &= name "anonymous"-}
                                  {-, fAnonymousWrite = False &= help "allow anonymous write operations, defaults to False" &= explicit &= name "anonymous-write"-}
                                  {-, fWritable       = True &= help "allow write operations, defaults to True" &= explicit &= name "writable"-}
                                  {-, fPort           = "21" &= help "service port, defaults to 21" &= explicit &= name "port"-}
                                  {-, fService        = "vsftpd" &= help "service to use for file sharing, defaults to vsftpd" &= explicit &= name "service"-}
                                  {-, fRecreateUser   = False &= help "recreate the user" &= explicit &= name "recreate-user"-}
                                  {-, fRemote = def &= help "path to the file containing list of remote addresses in the format: user@host:port"-}
                                  {-} &= explicit &= name "filesharing"-}


  {-install = InstallParams { iService = def &= argPos 0-}
                          {-, remote = def &= help "path to the file containing list of remote addresses in the format: user@host:port"-}
                          {-} &= explicit &= name "install"-}

  {-webserverSetup (WebServerParams { directory, domain, port, ssl, forward, wService, email, wRemote }) = do-}
    {-remoteSetup wRemote $ do-}
      {-when (ssl && null email) $ die "Email is required for generating a certificate"-}

      {-let serverType -}
            {-| (not . null) forward = S.PortForwarding-}
            {-| otherwise = S.Static-}

      {-let serviceName = read wService-}

      {-let portNumber-}
            {-| (not . null) port = port-}
            {-| ssl = "443"-}
            {-| otherwise = "80"-}

      {-absoluteDirectory <- makeAbsolute directory-}

      {-let params = S.ServerParams { S.wDirectory    = absoluteDirectory-}
                                  {-, S.domain        = domain-}
                                  {-, S.port          = portNumber-}
                                  {-, S.ssl           = ssl-}
                                  {-, S.forward       = forward-}
                                  {-, S.serverType    = serverType-}
                                  {-, S.serverService = serviceName-}
                                  {-, S.email         = email-}
                                  {-}-}
      {-return $ S.detectOS >>= (S.install serviceName)-}
            {->> S.detectOS >>= (S.start serviceName)-}
            {->> S.newServer params-}

  {-manualInstall (InstallParams { iService, remote }) =-}
    {-remoteSetup remote $ do-}
      {-let serviceName = read iService-}

      {-return $ S.detectOS >>= (S.install serviceName)-}
             {->> S.detectOS >>= (S.start serviceName)-}
    

  {-databaseSetup (DatabaseParams { databaseName, dService, dummyData, dUser, dPass, dHost, dRemote }) = do-}
    {-remoteSetup dRemote $ do-}
      {-let serviceName = read dService-}

      {-let params = S.DatabaseParams { S.database        = databaseName-}
                                    {-, S.databaseService = serviceName-}
                                    {-, S.dummyData       = dummyData-}
                                    {-, S.databaseUser    = dUser-}
                                    {-, S.databasePass    = dPass-}
                                    {-, S.databaseHost    = dHost-}
                                    {-}-}

      {-return $ S.detectOS >>= (S.install serviceName)-}
            {->> S.detectOS >>= (S.start serviceName)-}
            {->> S.newDatabase params-}

  {-fileSharingSetup (FileSharingParams { fDirectory, fUser, fPass, fPort, fAnonymous, fAnonymousWrite, fWritable, fService, fRecreateUser, fRemote }) = do-}
    {-remoteSetup fRemote $ do-}
      {-let serviceName = read fService-}

      {-let params = S.FileSharingParams { S.fDirectory      = fDirectory-}
                                      {-, S.fUser           = fUser-}
                                      {-, S.fPass           = fPass-}
                                      {-, S.fPort           = fPort-}
                                      {-, S.fAnonymous      = fAnonymous-}
                                      {-, S.fAnonymousWrite = fAnonymousWrite-}
                                      {-, S.fWritable       = fWritable-}
                                      {-, S.fService        = serviceName-}
                                      {-, S.fRecreateUser   = fRecreateUser-}
                                      {-}-}

      {-return $ S.detectOS >>= (S.install serviceName)-}
            {->> S.detectOS >>= (S.start serviceName)-}
            {->> S.newFileSharing params-}

  {-remoteSetup file generateAction-}
    {-| null file = do-}
      {-action <- generateAction-}
      {-S.runApp $-}
        {-S.run action-}

      {-return ()-}

    {-| otherwise = do-}
      {-list <- liftIO $ map read . lines <$> readFile file-}
      {-action <- generateAction-}
      {-S.runApp $ S.run $ S.remote list action-}

      {-return ()-}
 
