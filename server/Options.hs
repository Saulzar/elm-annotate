module Options where

import Common
import System.Console.CmdArgs.Implicit

default (Int, Float)

data Options = Options
  { root :: String
  , discard :: Bool
  } deriving (Data, Typeable, Show)


options :: Options
options =
  Options { root        = def  &= argPos 0
          , discard     = def  &= help "discard data and start fresh"
          } &= summary "Annotation server"


getArgs :: IO Options
getArgs = cmdArgs options
