module Scene.Viewport where

import Common
import Types
import Control.Lens hiding (zoom)

import Miso (View)
import qualified Miso.String as S
import Miso.String (MisoString)

import Svg

data Viewport = Viewport
  { bounds  :: Box
  , size    :: Size
  , pan     :: Position
  , zoom    :: Float
  } deriving (Generic, Eq, Show)


init :: Viewport
init = Viewport
  { zoom    = 1
  , pan     = V2 0 0
  , size    = V2 0 0
  , bounds  = Box (V2 0 0) (V2 0 0)
  }



panView :: Position -> Position -> Viewport -> Viewport
panView pos page view = view & #pan %~ (+ d)
  where d  = toLocal view page - pos


zoomView :: Float -> Position -> Viewport -> Viewport
zoomView = undefined


toLocal :: Viewport -> Position -> Position
toLocal view page = (page - pageOffset view) ^/ (zoom view)

pageOffset :: Viewport -> Position
pageOffset view = view ^. (#bounds . #lower) + localOffset view

localOffset :: Viewport -> Position
localOffset Viewport{..} = pan ^* zoom + 0.5 *^ (boxSize bounds + zoom *^ size)



view :: Viewport -> [View action] -> View action
view v@Viewport{..} inner = svg_ [ version_ "1.1", width_ w, height_ h, viewBox_ 0 0 w h ]
  [ g_ [transform_ [Translate tx ty, Scale zoom zoom]] inner ]
      where
          (V2 w h) = size
          (V2 tx ty) = localOffset v
