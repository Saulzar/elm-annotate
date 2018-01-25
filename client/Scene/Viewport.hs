module Scene.Viewport where

import Common
import Types
import Control.Lens hiding (zoom)

import Miso (View, class_)
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


zoomDelta :: Float -> Viewport -> Viewport
zoomDelta delta = #zoom %~ clamp (0.25, 4) . (* factor)
  where factor = (1 - delta / 500)

zoomView :: Float -> Position -> Viewport -> Viewport
zoomView amount pos view = panView pos page (zoomDelta amount view)
  where page = toPage view pos


toPage :: Viewport -> Position -> Position
toPage view local = pageOffset view + zoom view *^ local

toLocal :: Viewport -> Position -> Position
toLocal view page = (page - pageOffset view) ^/ (zoom view)

pageOffset :: Viewport -> Position
pageOffset view = view ^. (#bounds . #lower) + localOffset view

localOffset :: Viewport -> Position
localOffset Viewport{..} = pan ^* zoom + 0.5 *^ (boxSize bounds - zoom *^ size)



view :: Viewport -> [View action] -> View action
view v@Viewport{..} inner = svg_ [ version_ "1.1", class_ "expand" ]
  [ g_ [transform_ [Translate tx ty, Scale zoom zoom]] inner ]
      where
          (V2 w h) = boxSize bounds
          (V2 tx ty) = localOffset v
