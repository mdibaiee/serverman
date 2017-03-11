{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.VsFTPd (vsftpd) where
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Actions.FileSharing

  import System.Directory
  import System.IO
  import System.IO.Error
  import System.FilePath
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Free
  import Data.List
  import Data.Either
  import Control.Monad.State

  vsftpd :: FileSharingParams -> App ()
  vsftpd params@(FileSharingParams { fDirectory, fPort, fUser, fPass, fAnonymous, fAnonymousWrite, fWritable, fService, fRecreateUser }) =
    do
      let content = show params
          original = configDirectory fService
          userList = takeDirectory original </> "vsftpd-serverman-user-list"

      when fRecreateUser $ executeRoot "userdel" [fUser] "" True >> return ()

      (Right opensslResponse) <- execute "openssl" ["passwd", "-1", fPass] "" True
      let encryptedPassword = head . lines $ opensslResponse

      executeRoot "useradd" [fUser, "-d", fDirectory, "-G", "ftp", "-p", encryptedPassword] "" True

      liftIO $ do
        renameFileIfMissing original (original ++ ".backup")
        writeFile original content
        writeFile userList fUser

      result <- restartService "vsftpd"
      case result of
        Left err -> return ()
        Right _ ->
          liftIO $ putStrLn $ "restarted " ++ show fService
