module System.Serverman ( run
                        , module System.Serverman.Action
                        , module System.Serverman.Utils
                        , module System.Serverman.Services
                        , module System.Serverman.Actions.WebServer
                        , module System.Serverman.Actions.Database
                        , module System.Serverman.Actions.Env
                        , module System.Serverman.Actions.Install) where

  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services

  import System.Serverman.Actions.Install
  import System.Serverman.Actions.Env

  import System.Serverman.Actions.WebServer
  import System.Serverman.Actions.Nginx

  import System.Serverman.Actions.Database
  import System.Serverman.Actions.MySQL

  import Control.Monad.Free

  run :: Action r -> IO r
  run (Pure r) = return r
  run (Free (NewWebServer params next))
    | serverService params == NGINX = nginx params >> run next
    | otherwise = run next
  run (Free (DetectOS next)) = getOS >>= run . next
  run (Free (Install os service next)) = installService os service >> run next
  run (Free (NewDatabase params next))
    | databaseService params == MySQL = mysql params >> run next
    | otherwise = run next

