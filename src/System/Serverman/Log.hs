{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Serverman.Log ( verbose
                            , info
                            , write
                            , progress
                            , warning
                            , err
                            , die
                            , progressListener) where

  import System.Serverman.Types

  import Text.Termcolor
  import Text.Termcolor.Style
  import qualified Text.Termcolor.Foreground as F
  import qualified Text.Termcolor.Background as B
  import qualified System.Exit as E
  import Control.Concurrent.Async
  import Control.Monad.State
  import Control.Concurrent
  import System.IO
  import Control.Monad.Trans.Control

  verbose :: String -> App ()
  verbose str = do
    (AppState { verboseMode }) <- get
    liftIO $
      when verboseMode $ do
        putStrLn . format . F.gray $ read ("[verbose] " ++ str)

  write :: String -> App ()
  write str = liftIO . putStrLn . format . reset $ read str

  info :: String -> App ()
  info str = liftIO . putStrLn . format . reset $ read ("[info] " ++ str)

  warning :: String -> App ()
  warning str = liftIO . putStrLn . format . F.yellow $ read ("[warning] " ++ str)

  err :: String -> App ()
  err str = liftIO . putStrLn . format . bold . F.red $ read ("[error] " ++ str)

  die :: String -> App ()
  die str = liftIO . E.die . format . bold . F.red $ read ("[fatal error] " ++ str)

  progress :: App (App ())
  progress = do
    state <- get
    p <- progressListener

    return p


  progressPrefix = "working "
  progressCharacters = [".  ", ".. ", "...", " ..", "  .", "   "]
  progressDelay = 200000
  progressListener :: App (App ())
  progressListener = do
    p <- liftedAsync $
      mapM start (cycle [0..length progressCharacters])

    return $ stop p

    where
        start n = do
          liftIO . threadDelay $ progressDelay

          liftedAsync $ do
            let str = progressPrefix ++ (progressCharacters !! n)

            liftIO $ do
              putStr . format . (light . F.blue) $ read str
              putStr $ "\ESC[" ++ (show $ length str) ++ "D\ESC[0;"
              hFlush stdout

            return ()

        stop process = do
          liftIO $ do
            cancel process
            putStr "\ESC[0;"
