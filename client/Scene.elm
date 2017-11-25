module Scene exposing (..)

import Html exposing (..)

import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types as Svg exposing (..)
import TypedSvg  as Svg exposing (..)
import TypedSvg.Attributes as Svg exposing (..)
--
--
-- import Input exposing (Event(..))
import Vector as V exposing (Size, Position, Vec2, Box)
import Image exposing (Image)

import Scene.View as View exposing (Geometry)
import Scene.Settings as Settings exposing (Settings)

import Scene.Types exposing (..)
import Scene.Document as Doc

import Types exposing (..)


import Input
import Util exposing (..)
import Maybe
-- import Dict
-- import Debug
--import Util exposing (..)



empty : Scene
empty =
  { background = Nothing
  , view = View.init
  , settings = Settings.init
  , action = Inactive

  , doc = Doc.init

  }


modifyView : (Geometry -> Geometry) -> Scene -> Scene
modifyView f scene = {scene | view = f scene.view}

modifySettings : (Settings -> Settings) -> Scene -> Scene
modifySettings f scene  = {scene | settings = f scene.settings}


modifyDoc : (Document -> Document) -> Scene -> Scene
modifyDoc f scene  = {scene | doc = f scene.doc}

setBackground : Image -> Scene -> Scene
setBackground image scene = {scene | background = Just image, view = View.setSize image.size scene.view}

runCommand : Command -> Scene -> Scene
runCommand cmd = case cmd of
    Pan pos mouse  -> modifyView (View.pan pos mouse)
    Zoom zoom pos  -> modifyView (View.zoomTo zoom pos)
    ZoomBrush zoom -> modifySettings (Settings.zoomBrush zoom)
    MakeEdit e ->  modifyDoc (Doc.applyEdit e)



runMaybe : Maybe Command -> Scene -> Scene
runMaybe = applyMaybe runCommand

interact : (Input.Event, Input.State) -> Scene -> Scene
interact input scene = case scene.action of
  Inactive      -> scene
  Active action -> case action.update input scene of

    Continue update cmd -> runMaybe cmd {scene | action = Active update }
    End cmd   -> runMaybe cmd {scene | action = Inactive }
    Ignored   -> scene


setBounds : Box -> Scene -> Scene
setBounds box scene = {scene | view = View.setBounds box scene.view}

toLocal : Scene -> Position -> Position
toLocal scene = View.toLocal scene.view

maybeSvg : Maybe a -> (a -> Svg msg) -> Svg msg
maybeSvg ma f = case ma of
  Nothing -> g [] []
  Just  a -> f a


maybeAction : Scene -> Maybe Action
maybeAction scene = case scene.action of
  Inactive      -> Nothing
  Active action -> Just action

view : (Msg -> msg) -> Scene -> Html msg
view f scene = Html.map f <| View.view scene.view
  [ maybeSvg scene.background
      (\i -> image [xlinkHref i.src, x (px 0), y (px 0), width (px i.size.x), height (px i.size.y) ] [])

  , g [] (List.map viewObject scene.doc.instances)
  , maybeSvg (maybeAction scene)
      (\action -> Svg.map (always Ignore) <| action.view scene)
  ]


circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle [class ["brush"], cx (px pos.x), cy (px pos.y), r (px radius)] []

rect : Position -> Position -> Svg msg
rect p1 p2 =
  let size = V.sub p2 p1
  in Svg.rect [class ["brush"], x (px p1.x), y (px p1.y), width (px size.x), height (px size.y)] []


viewObject : Object -> Svg Msg
viewObject obj = case obj of
  Point p ->  circle p.position p.radius
  _       -> g [] []



zoomCentre : Scene -> Float -> Command
zoomCentre scene amount = Zoom amount (V.centre scene.view.bounds)


cancelAction : Scene -> Scene
cancelAction scene = { scene | action = Inactive }


update : Msg -> Scene -> Scene
update msg scene = case msg of
  Start action -> case scene.action of
    Inactive -> { scene | action = Active action }
    _        -> scene

  Run cmd -> runCommand cmd scene
  Cancel  -> cancelAction scene
  Ignore -> scene



-- view :
