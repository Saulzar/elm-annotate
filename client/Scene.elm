module Scene exposing (..)

import Html exposing (..)

import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types as Svg exposing (..)
import TypedSvg  as Svg exposing (..)
import TypedSvg.Attributes as Svg exposing (..)

--
--
-- import Input exposing (Event(..))
import Vector as V exposing (Size, Position, Vec2)
import Image exposing (Image)

import Scene.View as View exposing (Geometry)
import Scene.Settings as Settings exposing (Settings)

import Scene.Types exposing (..)
import Scene.Document as Doc
import Scene.Action as Action

import Types exposing (..)
import Common exposing (..)

import Input
import Input.Mouse as Mouse

import Util exposing (..)
import Maybe
import Dict
-- import Debug
--import Util exposing (..)



load : ClientId -> Document -> Image -> Scene -> Scene
load clientId doc img scene =
  let nextObj = 1 + Maybe.withDefault 0 (Doc.maxObject doc)

  in { scene | background = Just img
    , action      = Inactive

    , doc         = doc
    , nextId      = (nextObj, clientId)
    , selection   = []
    }


init : Scene
init =
  { settings = Settings.init
  , view     = View.init
  , action      = Inactive

  , background = Nothing
  , doc = Doc.init
  , nextId = (0, 0)
  , selection = []
  }


modifyView : (Geometry -> Geometry) -> Scene -> Scene
modifyView f scene = {scene | view = f scene.view}

modifySettings : (Settings -> Settings) -> Scene -> Scene
modifySettings f scene  = {scene | settings = f scene.settings}



modifyDoc : (Document -> Document) -> Scene -> Scene
modifyDoc f scene  = {scene | doc = f scene.doc}

incId : Scene -> Scene
incId scene = let incId (object, client) = (object + 1, client) in
  {scene | nextId = incId scene.nextId}

applyEdit : Edit -> Scene -> Scene
applyEdit e = modifyDoc (Doc.applyEdit e) >> incId

runCommand : Command -> Scene -> Scene
runCommand cmd = case cmd of
    Pan pos mouse  -> modifyView (View.pan pos mouse)
    Zoom zoom pos  -> modifyView (View.zoomTo zoom pos)
    ZoomBrush zoom -> modifySettings (Settings.zoomBrush zoom)
    MakeEdit e -> applyEdit e
    Select ids -> \scene -> {scene | selection = ids}


runMaybe : Maybe Command -> Scene -> Scene
runMaybe = applyMaybe runCommand

toMsg : Maybe Command -> List Msg
toMsg mCmd = case mCmd of
  Nothing  -> []
  Just cmd -> [Run cmd]



interact : (Input.Event, Input.State) -> Scene -> List Msg
interact (e, input) scene = case scene.action of
    Inactive      -> []
    Active action -> case action.update e input scene of
      Continue update cmd -> toMsg cmd ++ case update of
          Nothing     -> []
          Just action -> [Update action]

      End cmd   -> Cancel :: toMsg cmd
      Ignored   -> []


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


getPending : Scene -> List Edit
getPending scene = maybeAction scene |> Maybe.map (.pending) |> Maybe.withDefault []


view : (Msgs -> msg) -> Input.State -> Scene -> Html msg
view f input scene = Html.map f (view_ input <| List.foldr applyEdit scene (getPending scene))

svgImage : Image -> Svg msg
svgImage i = image [xlinkHref i.src, x (px 0), y (px 0), width (px i.size.x), height (px i.size.y) ] []

view_ : Input.State -> Scene -> Html Msgs
view_ input scene = View.view scene.view
    [ maybeSvg scene.background svgImage
    , g [] (List.map (viewObject scene input) (Dict.toList scene.doc.instances))
    , maybeSvg (maybeAction scene)
        (\action -> Svg.map (always []) <| action.view scene)
    ]



maybeWhen : (a -> Bool) -> b -> a ->  Maybe b
maybeWhen f b a = if f a then Just b else Nothing

equals : a -> a -> Bool
equals a b = a == b

circle : Position -> Float -> Svg msg
circle pos radius = Svg.circle
  [ class ["object"]
  , cx (px pos.x), cy (px pos.y), r (px radius)
  ] []

rect : Position -> Position -> Svg msg
rect p1 p2 =
  let size = V.sub p2 p1
  in Svg.rect [class ["object"], x (px p1.x), y (px p1.y), width (px size.x), height (px size.y)] []


isSelected : {a | selection : List ObjId } -> ObjId -> Bool
isSelected scene id = List.member id scene.selection


select : List ObjId -> Msg
select ids = Run (Select ids)

edit : Edit -> Msg
edit = Run << MakeEdit

viewObject : Scene -> Input.State -> (ObjId, Object) -> Svg Msgs
viewObject scene  input (id, obj) =
  let selected = isSelected scene id
      render = case obj of
        ObjPoint p ->  circle p.position p.radius
        _       -> g [] []

      local = View.toLocal scene.view input.position
      msg = [select [id], Start (Action.dragObjects Mouse.Left [id] local)]
  in g [ class (if selected then ["selected"] else [])
       , Mouse.onDown_ (maybeWhen (equals Mouse.Left) msg) ] [render]


zoomCentre : Scene -> Float -> Command
zoomCentre scene amount = Zoom amount (V.centre scene.view.bounds)


cancelAction : Scene -> Scene
cancelAction ed = { ed | action = Inactive }



update : Msg -> Scene -> Scene
update msg scene = case msg of
  Start action -> case scene.action of
    Inactive -> { scene | action = Active action }
    _        -> scene

  Update action -> case scene.action of
    Active _ -> { scene | action = Active action }
    _        -> scene

  Run cmd -> runCommand cmd scene
  Cancel  -> cancelAction scene
  Ignore -> scene





-- view :
