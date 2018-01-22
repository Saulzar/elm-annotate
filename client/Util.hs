module Util where

import Common

import qualified Miso.String as S
import Miso.String (MisoString)


show' :: Show a => a -> MisoString
show' = S.pack . show
