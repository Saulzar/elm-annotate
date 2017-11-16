module Drawing exposing (init, update, view, subscriptions, Model, Msg, setImage)


import Json.Decode as Json

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Window
import Scene
import Scene.Types exposing (Scene, Action, Command(..), Update(..))
import Scene.Action as Action

import View
import Image as Image exposing (Image)
import Input.Element as Element
import Input.Mouse as Mouse
--import Input.Window as Window

import Input

import Util exposing (..)

import Vector as V exposing (Size, Position, Vector, Box)

type Msg = Input Input.Event | Scene Scene.Msg | NeedsResize | ViewSize Box | Ignore


type alias Model = {
  view   : View.Geometry,
  input  : Input.State,
  scene  : Scene,
  action : Maybe Action
}


init : (Model, Cmd Msg)
init = let state = { view = View.init, input = Input.init, scene = Scene.empty, action = Nothing  }
  in (state, Cmd.batch [Element.askGeometry drawingId])

subscriptions : (Msg -> msg) -> Sub msg
subscriptions f =
  Sub.batch
  [ Input.subscriptions (Input >> f)
  , Window.resizes  (always NeedsResize >> f)
  , Sub.map f geometry
  ]


startAction :  Action -> Msg
startAction =  Scene.Start >> Scene

updateCommand : Maybe Command -> Model -> Model
updateCommand ma model = case ma of
  Nothing -> model
  Just act -> case act of
    Pan pos        -> {model | view = View.pan pos model.view}
    Zoom zoom pos  -> {model | view = View.zoom zoom pos model.view}




interact : (Input.Event, Input.State) -> Model -> (Model, Cmd Msg)
interact input model = case model.action of
  Nothing     -> noCmd model
  Just action -> case action.update input of

    Continue update act -> noCmd <| updateCommand act {model | action = Just update}
    End act   -> noCmd (updateCommand act {model | action = Nothing})
    Ignored   -> noCmd model




update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  ViewSize box  -> noCmd {model | view = View.setBounds box model.view}

  NeedsResize -> (model, Element.askGeometry drawingId)
  Input e     -> let
    local = Input.transform (View.toLocal model.view) e
    input = Input.update local model.input
    bound = Input.mapBindings (input, local)
      in  interact (bound, input) { model | input = input }


  Scene (Scene.Start action) -> case model.action of
    Nothing -> noCmd { model | action = Just action }
    _       -> noCmd model

  Ignore      -> noCmd model



drawingId : String
drawingId = "image_drawing"

geometry : Sub Msg
geometry = Element.geometry drawingId (\m -> case m of
    Nothing -> Ignore
    Just geom -> ViewSize geom)

setImage : Image -> Model -> Model
setImage image model = {model
  | view = View.setSize image.size model.view
  , scene = Scene.setBackground image model.scene
  }


onContextMenu : msg -> Attribute msg
onContextMenu msg = onWithOptions "contextmenu" { preventDefault = True, stopPropagation = True } (Json.succeed msg)


view : Model -> Html Msg
view model = div
  [ class "drawing", id drawingId
  -- , style [("cursor", Maybe.withDefault "default" (Maybe.map .cursor model.action))]
  --,  onContextMenu Ignore
  ,  tabindex 0

  , Mouse.onDown (\b -> case b of
        Mouse.Left -> startAction (Action.pan model.input.position)
        _          -> Ignore)

  , Mouse.onWheel (\deltas -> startAction (Action.zoom deltas.dy model.input.position))

  ]

  [ View.view model.view (Scene.view Scene  model.scene)
  ,  div [] [text (toString model)]
  ]
