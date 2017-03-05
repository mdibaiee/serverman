module System.Serverman.Actions.Remote ( runRemotely
                                       , Address) where
  import System.Serverman.Utils
  import Data.List
  import System.Directory
  import System.IO
  import System.FilePath

  type Host = String
  type Port = String
  type User = String
  data Address = Address Host Port User 

  runRemotely :: Address -> IO r -> IO ()
  runRemotely addr@(Address host port user) action = do
    let path = "/tmp/serverman/" </> show addr

    createDirectoryIfMissing True path

    execute "sshfs" [show addr, path] "" True

    return ()

  instance Read Address where
    readsPrec _ addr
      | '@' `elem` addr =
            let (user, rest) = span (== '@') addr
                (host, port) = readHostPort rest
            in [(Address host port user, [])]
      | otherwise = 
            let (host, port) = readHostPort addr
            in [(Address host port "", [])]

      where
        readHostPort str = span (== ':') str

  instance Show Address where
    show (Address host port user) = user ++ "@" ++ host ++ ":" ++ port
