module System.Term.Remote (handle) where
  import qualified System.Serverman as S

  handle file generateAction
    | null file = S.run =<< generateAction
    | otherwise = do
      list <- map read . lines <$> readFile file
      action <- generateAction

      S.run $ S.remote list action
