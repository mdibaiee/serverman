module System.Serverman ( run
                        , module System.Serverman.Action
                        , module System.Serverman.Utils
                        , module System.Serverman.Services
                        , module System.Serverman.Actions.Env
                        , module System.Serverman.Actions.Install) where

  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Types

  import System.Serverman.Actions.Env
  import System.Serverman.Actions.Install
  import System.Serverman.Actions.Manage
  import System.Serverman.Actions.Monitor
  import System.Serverman.Actions.Repository
  import System.Serverman.Actions.Remote
  import System.Serverman.Actions.Call

  import Control.Monad.Free

  import Debug.Trace

  run :: Action r -> App r
  run (Pure r) = return r
  run (Free (DetectOS next)) = getOS >> run next
  run (Free (Start service next)) = startService service >> run next
  run (Free (Stop service next)) = stopService service >> run next
  run (Free (Install service next)) = installService service >> run next

  run (Free (Call service remote next)) = callService service remote >> run next

  run (Free (Remote addrs action next)) = mapM_ (\addr -> unmountPath addr >> runRemotely addr (run action)) addrs >> run next

  run (Free (FetchRepository next)) = fetchRepo False >> run next
  run (Free (UpdateRepository next)) = fetchRepo True >> run next

  run (Free (Status service next)) = serviceStatus service >> run next
  run (Free (Log service next)) = serviceLogs service >> run next
