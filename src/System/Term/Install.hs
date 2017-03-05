{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Term.Install (mode, handle, Params(..)) where
  import System.Console.CmdArgs
  import qualified System.Serverman as S
  import qualified System.Term.Remote as R
  import Control.Monad
  import System.Exit
  import System.Directory

  data Params = Params { service   :: String
                       , remote    :: FilePath
                       } deriving (Show, Data, Typeable)

  mode = Params { service = def &= argPos 0
                , remote  = def &= help "file to read remote hosts from. each line should contain a host:port" &= typDir &= explicit &= name "remote"
                }

  handle (Params { service, remote }) =
    R.handle remote $ do
      let serviceName = read service

      return $ S.detectOS >>= (S.install serviceName)
             >> S.detectOS >>= (S.start serviceName)


