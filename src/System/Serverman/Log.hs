{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Serverman.Log ( verbose
                            , info
                            , write
                            , progress
                            , progressText
                            , warning
                            , err
                            , success
                            , die) where

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
    AppState { verboseMode } <- get
    liftIO $
      when verboseMode $
        putStrLn . format . F.gray $ read ("[verbose] " ++ str)

  write :: String -> App ()
  write str = liftIO . putStrLn . format . reset $ read str

  info :: String -> App ()
  info str = liftIO . putStrLn . format . reset $ read ("[info] " ++ str)

  success :: String -> App ()
  success str = liftIO . putStrLn . format . F.green $ read str

  warning :: String -> App ()
  warning str = liftIO . putStrLn . format . F.yellow $ read ("[warning] " ++ str)

  err :: String -> App ()
  err str = liftIO . putStrLn . format . bold . F.red $ read ("[error] " ++ str)

  die :: String -> App ()
  die str = liftIO . E.die . format . bold . F.red $ read ("[fatal error] " ++ str)

  progress :: App (App ())
  progress = progressText "working"

  clearLine :: IO ()
  clearLine = do
    putStr "\ESC[2K\ESC[0;"
    hFlush stdout
  
  backward :: Int -> IO ()
  backward n =
    putStr $ "\ESC[" ++ show n ++ "D\ESC[0;"

  progressText :: String -> App (App ())
  progressText = progressListener

  progressCharacters = [".  ", ".. ", "...", " ..", "  .", "   "]
  progressDelay = 200000
  progressListener :: String -> App (App ())
  progressListener text = do
    liftIO $ putStr $ replicate strLength '.'

    p <- liftedAsync $
      mapM start (cycle [0..length progressCharacters])

    return $ stop p

    where
        strLength = 2 + length text + length (head progressCharacters)
        start n = do
          liftIO . threadDelay $ progressDelay

          liftedAsync $ do
            let str = text ++ " " ++ (progressCharacters !! n)

            liftIO $ do
              backward strLength
              putStr . format . light . F.blue $ read str
              hFlush stdout

            return ()

        stop process = 
          liftIO $ do
            cancel process
            backward strLength
            clearLine
