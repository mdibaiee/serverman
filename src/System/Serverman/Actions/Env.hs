module System.Serverman.Actions.Env (OS(..), getOS) where
  import System.Serverman.Utils
  import System.Serverman.Types

  import System.Process
  import Data.List
  import System.IO.Error
  import Data.Either
  import Data.Char
  import Control.Monad.State
  
  getOS = do
    arch_release <- execute "cat" ["/etc/os-release"] "" False
    deb_release <- execute "cat" ["/etc/lsb-release"] "" False
    mac_release <- execute "sw_vers" ["-productName"] "" False

    let release = map toLower . head . rights $ [arch_release, deb_release, mac_release] 
        distro
            | or $ map (`isInfixOf` release) ["ubuntu", "debian", "raspbian"] = Debian
            | "arch" `isInfixOf` release = Arch
            | "Mac" `isInfixOf` release = Mac
            | otherwise = Unknown

    state <- get
    put $ state { os = distro }

    return ()
