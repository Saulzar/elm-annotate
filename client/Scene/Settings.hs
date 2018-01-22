module Scene.Settings where

import Common
import Types
import Geometry


data Settings = Settings
  { brushWidth :: Float
  } deriving (Generic, Show, Eq)

init :: Settings
init = Settings { brushWidth = 20 }



scaleBrush :: Float -> Settings -> Settings
scaleBrush zoom = #brushWidth %~ \r -> clamp (1, 200) (zoom * r)
