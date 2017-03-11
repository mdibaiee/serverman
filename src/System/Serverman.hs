module System.Serverman ( run
                        , module System.Serverman.Action
                        , module System.Serverman.Utils
                        , module System.Serverman.Services
                        , module System.Serverman.Actions.WebServer
                        , module System.Serverman.Actions.Database
                        , module System.Serverman.Actions.FileSharing
                        , module System.Serverman.Actions.Env
                        , module System.Serverman.Actions.Install) where

  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services

  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Install
  import System.Serverman.Actions.Start
  import System.Serverman.Actions.Remote

  import System.Serverman.Actions.WebServer
  import System.Serverman.Actions.Nginx

  import System.Serverman.Actions.Database
  import System.Serverman.Actions.MySQL
  import System.Serverman.Actions.MongoDB

  import System.Serverman.Actions.FileSharing
  import System.Serverman.Actions.VsFTPd

  import Control.Monad.Free

  run :: Action r -> App r
  run (Pure r) = return r
  run (Free (DetectOS next)) = getOS >>= run . next
  run (Free (Start os service next)) = startService os service >> run next
  run (Free (Install os service next)) = installService os service >> run next

  run (Free (NewWebServer params next))
    | serverService params == NGINX = nginx params >> run next
    | otherwise = run next

  run (Free (NewDatabase params next))
    | databaseService params == MySQL = mysql params >> run next
    | databaseService params == MongoDB = mongodb params >> run next
    | otherwise = run next

  run (Free (NewFileSharing params next))
    | fService params == VsFTPd = vsftpd params >> run next
    | otherwise = run next

  run (Free (Remote addrs action next)) = mapM_ (\addr -> runRemotely addr (run action)) addrs >> run next

