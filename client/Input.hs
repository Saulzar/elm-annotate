module Input where

import Miso.Subscription.Window


import Web.KeyCode hiding (KeyCode)

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Object
import           JavaScript.Object.Internal

import Geometry
import GHC.Generics

import qualified Data.Set as S
import Data.Set (Set)

import Data.Foldable
import Miso hiding (Key)

import Data.Aeson.Types


data Event = KeyDown Key | KeyUp Key | MouseMove (Int, Int) | Focus Bool

data State = State
  { keys :: Set Key
  , mouse :: (Int, Int)

  } deriving (Show, Generic, Eq)


initial :: State
initial = State
  { keys  = S.empty
  , mouse = (0, 0)
  }


clientDecoder :: Decoder (Int, Int)
clientDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o ->
       (,) <$> (o .: "clientX") <*> (o .: "clientY")

fromKeyCode :: KeyCode -> Key
fromKeyCode (KeyCode code) = keyCodeLookup code

subs :: (Event -> action) -> [Sub action model]
subs f =
  [ windowOn "mousemove" clientDecoder (f . MouseMove)
  , windowOn "keydown" keycodeDecoder (f . KeyDown . fromKeyCode)
  , windowOn "keyup" keycodeDecoder (f . KeyUp . fromKeyCode)
  , windowOn "focus" emptyDecoder (f . const (Focus True))
  , windowOn "blur" emptyDecoder (f . const (Focus False))

  ]
