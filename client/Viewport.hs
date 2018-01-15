module Viewport where

import Types
import Control.Lens hiding (zoom)


data Viewport = Viewport
  { bounds  :: Box
  , size    :: Size
  , pan     :: Position
  , zoom    :: Float
  } deriving (Generic, Show)


init :: Viewport
init = Viewport
  { zoom    = 1
  , pan     = V2 0 0
  , size    = V2 0 0
  , bounds  = Box (V2 0 0) (V2 0 0)
  }



panViewport :: Position -> Position -> Viewport -> Viewport
panViewport pos page view = view & #pan %~ (+ d)
  where d  = toLocal view page - pos


toLocal :: Viewport -> Position -> Position
toLocal view page = (page - pageOffset view) ^/ (zoom view)

pageOffset :: Viewport -> Position
pageOffset view = view ^. (#bounds . #lower) + localOffset view

localOffset :: Viewport -> Position
localOffset (Viewport {..}) = pan ^* zoom + 0.5 *^ (boxSize bounds + zoom *^ size)
