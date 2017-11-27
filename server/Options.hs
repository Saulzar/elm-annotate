module Options where

import System.Console.CmdArgs.Implicit

default (Int, Float)

data Options = Options
  { root :: String
  , create :: Bool
  } deriving (Data, Typeable, Show)


options :: Options
options =
  Options { root        = def   &= argPos 0
          , create      = False &= help "Initialise with default config file"
          } &= summary "Annotation server"


getArgs :: IO Options
getArgs = cmdArgs options
