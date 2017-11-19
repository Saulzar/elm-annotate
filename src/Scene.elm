module Scene exposing (..)

import Html exposing (..)

import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types as Svg exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
--
--
-- import Input exposing (Event(..))
import Vector as V exposing (Size, Position, Vector, Box)
import Image exposing (Image)

import Scene.View as View exposing (Geometry)
import Scene.Settings as Settings exposing (Settings)

import Scene.Types exposing (..)
import Scene.Document as Doc exposing (Document)


import Input
import Util exposing (..)
import Maybe
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
    Zoom zoom pos  -> modifyView (View.zoom zoom pos)
    ZoomBrush zoom -> modifySettings (Settings.zoomBrush zoom)
    MakeEdit e -> modifyDoc (Doc.edit e)




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
  , maybeSvg (maybeAction scene)
      (\action -> Svg.map (always Ignore) <| action.view scene)
  ]





update : Msg -> Scene -> Scene
update msg scene = case msg of
  Start action -> case scene.action of
    Inactive -> { scene | action = Active action }
    _        -> scene

  Run cmd -> runCommand cmd scene
  Cancel  -> { scene | action = Inactive }
  Ignore -> scene



-- view :
