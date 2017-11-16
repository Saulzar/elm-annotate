module Scene exposing (..)


import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
--
--
-- import Input exposing (Event(..))
-- import Vector as V exposing (Size, Position, Vector, Box)
import Image exposing (Image)

-- import Input
import Scene.Types exposing (..)

import Maybe
--import Util exposing (..)

type Msg = Start Action

empty : Scene
empty = { background = Nothing }


setBackground : Image -> Scene -> Scene
setBackground i scene = {scene | background = Just i}




view : (Msg -> msg) -> Scene -> Svg msg
view f scene = g []
  [ case scene.background of
      Just i  -> image [xlinkHref i.src, x (px 0), y (px 0), width (px i.size.x), height (px i.size.y) ] []
      Nothing -> g [] []

  -- , rect [x (px 0), y (px 0), width (px i.size.x), height (px i.size.y)] []
  ]

-- view :
