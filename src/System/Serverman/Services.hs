{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module System.Serverman.Services ( Service(..)
                                 , Repository
                                 , packageByOS
                                 , info) where
  import System.Serverman.Utils
  import System.Serverman.Actions.Env
  import System.Serverman.Types

  import Data.Aeson
  import Data.Maybe
  import GHC.Generics

  packageByOS :: Service -> OS -> [String]
  packageByOS (Service { packages }) os = fromMaybe (fromJust $ lookup Unknown packages) (lookup os packages)

  info :: Service -> String
  info s@(Service { config, packages, service, version, dependencies }) = 
    show s ++ (
      indent $
        keyvalue [ ("config", config)
                 , ("pacakges", commas $ map (commas . snd) packages)
                 , ("service", service)
                 , ("dependencies", commas $ map name dependencies)] ": "
    )
