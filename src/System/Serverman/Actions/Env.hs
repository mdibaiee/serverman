module System.Serverman.Actions.Env (OS(..), getOS, releaseToOS) where
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

    let release = map toLower . head . rights $ [arch_release, deb_release] 
        distro = releaseToOS release

    state <- get
    put $ state { os = distro }

    return ()

  releaseToOS :: String -> OS
  releaseToOS release
    | or $ map (`isInfixOf` release) ["ubuntu", "debian", "raspbian"] = Debian
    | "arch" `isInfixOf` release = Arch
    | otherwise = Unknown
