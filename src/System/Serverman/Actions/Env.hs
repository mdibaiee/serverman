module System.Serverman.Actions.Env (OS(..), getOS) where
  import System.Process
  import Data.List
  import System.IO.Error
  import Data.Either

  data OS = Debian | Arch | Unknown deriving (Show, Eq)
  
  getOS = do
    arch_release <- tryIOError $ readProcessWithExitCode "/usr/bin/cat" ["/etc/os-release"] ""
    deb_release <- tryIOError $ readProcessWithExitCode "/usr/bin/cat" ["/etc/lsb-release"] ""
    -- mac_release <- tryIOError $ readProcessWithExitCode "/usr/bin/sw_vers" ["-productVersion"] ""

    let (_, release, _) = head $ rights [arch_release, deb_release, mac_release] 
        distro
            | or $ map (`isInfixOf` release) ["ubuntu", "debian", "raspbian"] = Debian
            | "arch" `isInfixOf` release = Arch
            | otherwise = Unknown

    return distro
