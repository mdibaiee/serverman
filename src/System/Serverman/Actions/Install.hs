{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Serverman.Actions.Install (installService) where
  import System.Serverman.Action
  import System.Serverman.Utils
  import System.Serverman.Services
  import System.Serverman.Actions.Env
  import System.Serverman.Types

  import System.IO.Error
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.State
  import Control.Monad.Trans.Control

  installService :: Service -> OS -> App ()
  installService s@(Service { dependencies, packages }) os = do
    forM_ dependencies (`installService` os) 

    let base = case os of
          Arch -> ("pacman", ["-S", "--noconfirm", "--quiet"])
          Debian -> ("apt-get", ["install", "-y"])
          Mac -> ("brew", ["install", "-y"])
          _ -> ("echo", ["Unknown operating system"])
        pkg = packageByOS s os

    process <- liftedAsync $ do
      result <- executeRoot (fst base) (snd base ++ pkg) "" True

      case result of
        Left err -> return ()
        Right _ -> do
          liftIO $ putStrLn $ "installed " ++ show s ++ "."
      
    liftIO $ wait process
    return ()
