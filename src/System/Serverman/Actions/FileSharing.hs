{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.FileSharing (FileSharingParams(..)) where
  import System.Serverman.Services
  import System.Serverman.Utils

  data FileSharingParams = FileSharingParams { fDirectory      :: FilePath
                                             , fUser           :: String
                                             , fPass           :: String
                                             , fPort           :: String
                                             , fWritable       :: Bool
                                             , fAnonymous      :: Bool
                                             , fAnonymousWrite :: Bool
                                             , fRecreateUser   :: Bool
                                             , fService        :: Service
                                             } deriving (Eq)

  instance Show FileSharingParams where
    show (FileSharingParams { fDirectory, fUser, fPass, fPort, fWritable, fAnonymous, fAnonymousWrite, fService })
      | name fService == "vsftpd" = 
          let boolToEnglish True  = "YES"
              boolToEnglish False = "NO"
          in 
            keyvalue [ ("anonymous_enable", boolToEnglish fAnonymous)
                      , ("write_enable", boolToEnglish fWritable)
                      , ("allow_writeable_chroot", boolToEnglish fWritable)
                      , ("anon_upload_enable", boolToEnglish fAnonymousWrite)
                      , ("anon_mkdir_write_enable", boolToEnglish fAnonymousWrite)
                      , ("listen", "YES")
                      , ("userlist_enable", "YES")
                      , ("userlist_file", "/etc/vsftpd-serverman-user-list")
                      , ("userlist_deny", "NO")
                      , ("chroot_local_user", "YES")
                      , ("xferlog_enable", "YES")
                      , ("local_enable", "YES")] "="

      | otherwise = "Unknown service provider"
