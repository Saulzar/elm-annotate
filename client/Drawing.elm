module Drawing exposing (init, update, view, subscriptions, Model, Msg(..), setImage)


import Json.Decode as Json

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Window
import Scene
import Scene.Types as Scene exposing (Scene, Action, Command(..), Update(..))
import Scene.Action as Action




import Image as Image exposing (Image)
import Input.Element as Element
import Input.Mouse as Mouse
--import Input.Window as Window

import Keyboard.Key as Key exposing (Key)
import Input

import Util exposing (..)

import Vector as V exposing (Size, Position, Vec2, Box)

type Msg = Input Input.Event | Scene Scene.Msg | NeedsResize | ViewSize Box | Ignore




type alias Model = {
  input  : Input.State,
  scene  : Scene
}


init : (Model, Cmd Msg)
init = let state = { input = Input.init, scene = Scene.empty  }
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

runCommand :  Command -> Msg
runCommand =  Scene.Run >> Scene


modifyScene : (Scene -> Scene) -> Model -> Model
modifyScene f model = {model | scene = f model.scene}


bind : Key -> List Key -> a -> (Input.Binding, a)
bind k mod = (,) (Input.Binding k mod)

checkKeys : (Input.Event, Input.State) -> Scene -> Scene
checkKeys (e, state) scene =
  let match = Input.matchKeys (e, state)
         [ bind Key.Escape [] Scene.Cancel
         , let key = Key.Shift in
            bind key [] <| Scene.Start (Action.drawPoints key <| Scene.toLocal scene state.position)
         ]
  in applyMaybe Scene.update match scene


updateInput : (Input.Event, Input.State) -> Model -> Model
updateInput (event, input)  = modifyScene <| case event of
      Input.Focus _ ->  Scene.cancelAction
      _ ->  Scene.interact (event, input) >> checkKeys (event, input)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  ViewSize box  -> noCmd (modifyScene (Scene.setBounds box) model)

  NeedsResize -> (model, Element.askGeometry drawingId)
  Input event ->
    let input = Input.update event model.input
    in noCmd <| updateInput (event, input) { model | input = input }


  Scene cmd   -> noCmd (modifyScene (Scene.update cmd) model)
  Ignore      -> noCmd model



drawingId : String
drawingId = "drawingArea"

geometry : Sub Msg
geometry = Element.geometry drawingId (\m -> case m of
    Nothing -> Ignore
    Just geom -> ViewSize geom)

setImage : Image -> Model -> Model
setImage image model = {model | scene = Scene.setBackground image model.scene}


onContextMenu : msg -> Attribute msg
onContextMenu msg = onWithOptions "contextmenu" { preventDefault = True, stopPropagation = True } (Json.succeed msg)

localPosition : Model -> Position
localPosition model = Scene.toLocal model.scene model.input.position

events : Model -> List (Attribute Msg)
events model =
  [ Mouse.onDown (\b -> case b of
        Mouse.Left -> startAction (Action.pan (localPosition model))
        _          -> Ignore)

  , Mouse.onWheel (\deltas ->
      runCommand (Zoom  deltas.dy (localPosition model)))
  ]


view : (Msg -> msg) -> List (Html msg) -> Model -> Html msg
view f overlays model =
  let scene = Html.map f <| div (events model) [Scene.view Scene model.scene]
  in div [ class "drawing", id drawingId,  tabindex 0] (scene :: overlays)
