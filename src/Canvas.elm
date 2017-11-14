module Canvas exposing (init, update, view, subscriptions, Model, Msg, setImage)


import Json.Decode as Json

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- import Svg as Svg
-- import Svg.Attributes as Svg
import Input
import Window
import Interaction exposing (Interaction)
import Scene exposing (Scene)

import Image as Image exposing (ViewGeometry)


import Input.Size as Size


import Vector as V exposing (Size, Position, Vector, Box)

type Msg = Start Interaction | Input Input.Event | NeedsResize | ViewSize Box | ImageLoaded Size | Ignore


type alias Model = {
  view : ViewGeometry,
  image : Image.Model,
  input : Input.State,
  interaction : Maybe Interaction,
  scene : Scene
}


init : (Model, Cmd Msg)
init = let state = { view = Image.initView, image = Image.init, input = Input.init, interaction = Nothing, scene = Scene.empty  }
  in (state, Cmd.batch [Size.askGeometry canvasId])

subscriptions : (Msg -> msg) -> Sub msg
subscriptions f =
  Sub.batch
  [ Input.subscriptions (Input >> f)
  , Window.resizes  (always NeedsResize >> f)
  , Sub.map f geometry
  ]

none : model -> (model, Cmd Msg)
none model = (model, Cmd.none)

interact : Input.Event -> Model -> Model
interact e model = model



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  ImageLoaded size   -> none {model | image = Image.setSize size model.image}
  ViewSize box  -> none {model | view = Image.setView box model.view}

  NeedsResize -> (model, Size.askGeometry canvasId)
  Input e     -> (interact e  >> none) {model | input = Input.update e model.input }
  Start i     -> case model.interaction of
    Nothing -> none { model | interaction = Just i }
    _       -> none model
  Ignore      -> none model


canvasId : String
canvasId = "image_canvas"

geometry : Sub Msg
geometry = Size.geometry canvasId (\m -> case m of
    Nothing -> Ignore
    Just geom -> ViewSize geom)

setImage : String -> Model -> Model
setImage url model = {model | image = Image.setSrc url model.image }


onContextMenu : msg -> Attribute msg
onContextMenu msg = onWithOptions "contextmenu" { preventDefault = True, stopPropagation = True } (Json.succeed msg)


view : Model -> Html Msg
view model = div [class "canvas", id canvasId, onContextMenu Ignore, tabindex 0]
  [ Image.view ImageLoaded model.view model.image

  -- ,  Svg.svg [Svg.version "1.1"]
  --     [ Svg.circle [Svg.cx "100", Svg.cy "100", Svg.r "100"] []
  --     ]


  ,  div [] [text (toString model)]
  ]
