module Image exposing (..)

import Html.Events exposing (on)
import Html exposing (..)
import Html.Attributes exposing (..)


import Json.Decode as Json
import Vector as V exposing (Size, Position, Vector, Box)


type alias ViewGeometry = { geometry : Box, pan : Position, zoom : Float }

initView : ViewGeometry
initView = { zoom = 1, pan = Vector 0 0, geometry = Box (Vector 0 0) (Vector 0 0) }

type alias Model =
  { src : String
  , size : Maybe Size
  }

init : Model
init = { src = "", size = Nothing }

view : (Size -> msg) -> ViewGeometry -> Model -> Html msg
view f view model = img [src model.src, style (makePosition view model), draggable "false", onLoad f] []

setView : Box -> ViewGeometry -> ViewGeometry
setView b geom = {geom | geometry = b}


setSize : Size -> Model -> Model
setSize size model = { model | size = Just size }

setSrc : String -> Model -> Model
setSrc url model = if model.src == url then model else { model | src = url, size = Nothing }

geometry :  V.Size -> ViewGeometry  -> (V.Position, V.Size)
geometry  image view = let
    centered = V.scale 0.5 (V.sub view.geometry.size (V.scale view.zoom image))
    panned = V.add centered (V.scale view.zoom view.pan)
  in (panned, V.scale view.zoom image)

pixels : Float -> String
pixels x = toString x ++ "px"

makePosition : ViewGeometry -> Model -> List (String, String)
makePosition view model = case model.size of
  Nothing   -> [("display", "none")]
  Just imageSize ->
    let (pos, size) = geometry imageSize view
    in [("left", pixels pos.x), ("top", pixels pos.y), ("width", pixels size.x), ("height", pixels size.y)]



decodeImageSize  : Json.Decoder Size
decodeImageSize = Json.field "target" decodeSize

decodeSize : Json.Decoder Size
decodeSize = Json.map2 Vector
    (Json.field "width" Json.float)
    (Json.field "height" Json.float)


onLoad : (Size -> msg) -> Attribute msg
onLoad f = on "load" (Json.map f decodeImageSize)
