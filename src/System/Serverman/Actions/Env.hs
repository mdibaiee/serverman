module System.Serverman.Actions.Env (OS(..), getOS) where
  import System.Serverman.Utils
  import System.Process
  import Data.List
  import System.IO.Error
  import Data.Either

  data OS = Debian | Arch | Mac | Unknown deriving (Show, Eq)
  
  getOS = do
    arch_release <- execute "cat" ["/etc/os-release"] "" False
    deb_release <- execute "cat" ["/etc/lsb-release"] "" False
    mac_release <- execute "sw_vers" ["-productName"] "" False

    let release = head $ rights [arch_release, deb_release, mac_release] 
        distro
            | or $ map (`isInfixOf` release) ["ubuntu", "debian", "raspbian"] = Debian
            | "arch" `isInfixOf` release = Arch
            | "Mac" `isInfixOf` release = Mac
            | otherwise = Unknown

    return distro
