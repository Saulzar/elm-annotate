module Scene.Settings exposing (..)

import Vector as V exposing (Size, Position, Vec2, Box)
 

type alias Settings =
  { brushRadius : Float
  }


init : Settings
init =
  { brushRadius = 20
  }

zoomBrush : Float ->  Settings -> Settings
zoomBrush zoom  settings = {settings | brushRadius = V.clamp (1, 200) (zoom * settings.brushRadius)}
