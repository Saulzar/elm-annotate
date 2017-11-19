module Scene.View exposing (..)

import Vector exposing(..)


import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)

import Html exposing (..)
import Debug

import Vector as V exposing (..)

type alias Geometry = { bounds : Box, size : Size, pan : Position, zoom : Float }

init : Geometry
init = { zoom = 1, pan = Vector 0 0, size = Vector 0 0, bounds = Box (Vector 0 0) (Vector 0 0) }

setBounds : Box -> Geometry -> Geometry
setBounds b geom = {geom | bounds = b}

setSize : Size -> Geometry -> Geometry
setSize s geom = {geom | size = s}

pan : Position -> Position -> Geometry -> Geometry
pan pos page geom = let
  dPos  = V.sub (toLocal geom page) pos
    in {geom | pan = V.add geom.pan dPos}

zoom : Float -> Position -> Geometry -> Geometry
zoom zoom pos geom = let
    factor = 1 - zoom / 500
    page = toPage geom pos
  in  {geom | zoom = V.clamp (0.25, 4) (geom.zoom * factor)}



toLocal : Geometry -> Position -> Position
toLocal geom page = V.scale (1/geom.zoom) (V.sub page (pageOffset geom))


toPage : Geometry -> Position -> Position
toPage geom local = V.add (pageOffset geom) (V.scale geom.zoom local)

pageOffset : Geometry -> Position
pageOffset geom = V.add geom.bounds.position (localOffset geom)

localOffset : Geometry -> Position
localOffset geom =  let
    centered = V.scale 0.5 (V.sub geom.bounds.size (V.scale geom.zoom geom.size))
      in V.add centered (V.scale geom.zoom geom.pan)

view : Geometry -> List (Svg msg) -> Html msg
view geom inner = let
  t = localOffset geom
  s = geom.bounds.size
    in svg [ version "1.1", width (px s.x), height (px s.y), viewBox 0 0 s.x s.y ]
        [ g [transform [Translate t.x t.y, Scale geom.zoom geom.zoom]]  inner
        ]



-- geometry :  V.Size -> ViewGeometry  -> (V.Position, V.Size)
-- geometry  image view = let
--     centered = V.scale 0.5 (V.sub view.geometry.size (V.scale view.zoom image))
--     panned = V.add centered (V.scale view.zoom view.pan)
--   in (panned, V.scale view.zoom image)
