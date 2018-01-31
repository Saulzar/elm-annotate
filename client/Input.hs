module Input
  ( Key
  , module Input
  ) where

import Common

import Miso.Subscription.Window
import Web.KeyCode hiding (KeyCode)

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Object
import           JavaScript.Object.Internal

import qualified Data.Set as S
import Miso hiding (Key)

import Data.Aeson.Types hiding (defaultOptions)
import qualified Data.Aeson.Types as A

import Geometry

import Debug.Trace

data Event
  = MouseWheel Float
  | MouseDown Button
  | MouseUp Button
  | Click Button
  | KeyDown Key
  | KeyUp Key
  | MouseMove Position
  | Focus Bool
    deriving (Show, Eq, Ord, Generic)

data Button
  = LeftButton
  | MiddleButton
  | RightButton
  | OtherButton Int
    deriving (Show, Eq, Ord, Generic)


data State = State
  { keys :: Set Key
  , mouse :: Position

  } deriving (Show, Generic, Eq)


init :: State
init = State
  { keys  = S.empty
  , mouse = V2 0 0
  }


update :: Event -> State -> State
update (Focus   _) = #keys .~ S.empty
update (KeyDown k) = #keys %~ S.insert k
update (KeyUp   k) = #keys %~ S.delete k
update (MouseMove p) = #mouse .~ p
update _ = id




toButton :: Int -> Button
toButton 0 = LeftButton
toButton 1 = MiddleButton
toButton 2 = RightButton
toButton n = OtherButton n


instance Functor Decoder where
  fmap f d = d { decoder = \val -> f <$> decoder d val }


eventDecoder :: (A.Object -> Parser a) -> Decoder a
eventDecoder f = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" f



clientDecoder :: Decoder Position
clientDecoder = dimVector <$> eventDecoder (\o -> (,) <$> (o .: "clientX") <*> (o .: "clientY"))

buttonDecoder :: Decoder Button
buttonDecoder = eventDecoder (\o -> toButton <$> (o .: "button"))


fromKeyCode :: KeyCode -> Key
fromKeyCode (KeyCode code) = keyCodeLookup code


wheelDecoder :: Decoder Float
wheelDecoder = eventDecoder (\o -> o .: "deltaY")

windowOn' = windowOnWithOptions (Options { preventDefault = True, stopPropagation = True })
on' = onWithOptions (Options { preventDefault = True, stopPropagation = True })


subs :: (Event -> action) -> [Sub action model]
subs f =
  [ windowOn "mousemove"  clientDecoder  (f . MouseMove)
  , windowOn "keydown"    keycodeDecoder (f . KeyDown . fromKeyCode)
  , windowOn "keyup"      keycodeDecoder (f . KeyUp . fromKeyCode)
  , windowOn "focus"      emptyDecoder   (f . const (Focus True))
  , windowOn "blur"       emptyDecoder   (f . const (Focus False))
  , windowOn "mousedown"  buttonDecoder  (f . MouseDown)
  , windowOn "mouseup"    buttonDecoder  (f . MouseUp)
  , windowOn "click"      buttonDecoder  (f . Click)
  , windowOn' "wheel"      wheelDecoder   (f . MouseWheel)

  ]
