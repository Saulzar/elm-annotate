module Main exposing (main)

import Html exposing (..)
import Html.Lazy as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Navigation as Nav

import FontAwesome.Web as FA

import Image exposing (Image)
import Network

import Scene.Types as Scene exposing (Scene, Action, Active(..), Command(..))
import Types exposing (Dataset, ImageInfo, Response(..), Request (..), Edit(..))

import Input.Mouse as Mouse
import Input.Element as Element
import Scene.Action as Action

import Keyboard.Key as Key exposing (Key)

import Window
import Input
import Scene

import Json.Decode as Json
import Vector exposing (Position, Box)

import Util exposing (..)
import Tuple exposing (..)
-- import Debug

main : Program Never Model Msg
main =
    Nav.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


-- You need to keep track of the view state for the navbar in your model

type alias Model =
    { dataset : Maybe Dataset

    , selectedFile : Maybe String
    , location : Nav.Location

    , network : Network.State

    , input  : Input.State
    , scene  : Scene

    , activeTab : Maybe String
    , sortMethod : Int

    , sortedImages : List ImageInfo
    }


type Msg
    = Select String
    | ImageLoaded Image
    | UrlChange Nav.Location
    | Network Network.Msg
    | Scene (List Scene.Msg)

    | WindowResized
    | Input Input.Event
    | ViewSize Box

    | ShowTab (Maybe String)
    | SetSort Int
    | Ignore






init : Nav.Location -> ( Model, Cmd Msg )
init loc = let
    (network, netCmd) = Network.init loc

    model = { scene = Scene.empty
            , input = Input.init
            , network = network
            , dataset = Nothing
            , selectedFile = Nothing
            , location = loc

            , sortMethod = 0
            , activeTab = Just "Images"

            , sortedImages = []
            }
    cmds  = Cmd.batch
      [ Cmd.map Network netCmd
      , Network.request network.host ReqDataset
      , Element.askGeometry drawingId
      ]

  in  (model, cmds)




imagePath : Dataset -> String -> String
imagePath dataset file = dataset.path ++ "/" ++ file


modifyScene : (Scene -> Scene) -> Model -> Model
modifyScene f model = {model | scene = f model.scene}


handleResponse : Maybe Response -> Model -> (Model, Cmd Msg)
handleResponse r model = case r of
  (Just (RespDataset d)) -> let m = updateSort {model | dataset = Just d} in
    case List.head d.images of
      Nothing -> noCmd m
      Just info -> selectImage (info.file) m

  (Just (RespOpen file doc)) -> noCmd (modifyScene (Scene.loadDocument doc) model)
  _ -> noCmd model


selectImage : String -> Model -> (Model, Cmd Msg)
selectImage file model = case model.dataset of
  Just dataset -> ({ model | selectedFile = Just file, scene = Scene.clear model.scene },
    Cmd.batch [Image.loadImage (imagePath dataset file), Network.request model.network.host (ReqOpen file)] )
  Nothing -> noCmd model




getEdits : Scene.Msg -> Maybe Edit
getEdits msg = case msg of
  (Scene.Run (MakeEdit e)) -> Just e
  _   -> Nothing


editRequest : Model -> Edit -> Cmd msg
editRequest model e = Network.request model.network.host (ReqEdit e)

requestEdits : Model -> Scene.Msg -> Cmd msg
requestEdits model msg =  Maybe.withDefault Cmd.none <|
  Maybe.map (editRequest model) (getEdits msg)

sceneMsg : Scene.Msg -> Model -> (Model, Cmd Msg)
sceneMsg msg model = (modifyScene (Scene.update msg) model, requestEdits model msg)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Select file -> selectImage file model

    ImageLoaded info -> noCmd { model | scene = Scene.setBackground info model.scene }
    UrlChange _ -> noCmd model
    Network m ->
      let (state, resp, cmd) = Network.update m model.network
      in addCmd  (Cmd.map Network cmd) (handleResponse resp {model | network = state})

    ViewSize box  -> noCmd (modifyScene (Scene.setBounds box) model)
    WindowResized -> (model, Element.askGeometry drawingId)

    Scene msgs -> foldUpdate sceneMsg model msgs
    Input event ->
      let input = Input.update event model.input
          msgs = globalInput (event, input) model.scene
      in foldUpdate sceneMsg {model | input = input}  msgs


    ShowTab option -> noCmd {model | activeTab = option}
    SetSort method -> noCmd <| updateSort {model | sortMethod = method}
    Ignore -> noCmd model




subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Image.imageLoaded ImageLoaded
    , Network.subscriptions model.network Network

    , Input.subscriptions Input
    , Window.resizes  (always WindowResized)
    , subGeometry
    ]



-- Keyboard input

bind : Key -> List Key -> a -> (Input.Binding, a)
bind k mod = (,) (Input.Binding k mod)


bindWhen : Bool -> Scene.Msg -> Scene.Msg
bindWhen b msg = if b then msg else Scene.Ignore


checkKeys : (Input.Event, Input.State) ->  Scene -> Maybe Scene.Msg
checkKeys (e, state) scene = Input.matchKeys (e, state)
   [ bind Key.Escape [] Scene.Cancel
   , bind Key.Shift []  <|
      Scene.Start (Action.drawPoints Key.Shift <| Scene.toLocal scene state.position)

   , bind Key.Delete [] <|
      bindWhen (not (List.isEmpty scene.selection)) (Scene.edit (Many (List.map Delete scene.selection)))
   ]

globalInput : (Input.Event, Input.State) -> Scene -> List Scene.Msg
globalInput (event, input) scene  = case event of
      Input.Focus _ ->  [Scene.Cancel]
      _ ->  Scene.interact (event, input) scene ++ (maybeToList <| checkKeys (event, input) scene)


-- Drawing area


drawingId : String
drawingId = "drawing"


classes : List String -> Attribute msg
classes xs = class <| String.join " " xs

subGeometry : Sub Msg
subGeometry = Element.geometry drawingId (\m -> case m of
    Nothing -> Ignore
    Just geom -> ViewSize geom)


onContextMenu : msg -> Attribute msg
onContextMenu msg = onWithOptions "contextmenu" { preventDefault = True, stopPropagation = True } (Json.succeed msg)

localPosition : Model -> Position
localPosition model = Scene.toLocal model.scene model.input.position

startAction : Action -> Msg
startAction = Scene.Start >> List.singleton >> Scene

runCommand : Command -> Msg
runCommand = Scene.Run >> List.singleton >> Scene


events : Model -> List (Attribute Msg)
events model =
  [ Mouse.onDown (\b -> case b of
        Mouse.Left -> Scene [Scene.select [], Scene.Start (Action.pan (localPosition model))]
        _          -> Ignore)

  , Mouse.onWheel (\deltas ->
      runCommand (Zoom  (Mouse.zoomBy deltas) (localPosition model)))
  ]


cursorAttribs : Active -> (String, String, String)
cursorAttribs ma = case ma of
  Inactive      -> ("", "auto", "auto")
  Active action -> ("cursor_lock", action.cursor, "none")



view : Model -> Html Msg
view model =
  let (cursor_class, cursor, pointer_events) = cursorAttribs model.scene.action in

    div [draggable "false", style [("cursor", cursor)]]
        [ div [classes ["expand horiz", cursor_class], style [("pointer-events", pointer_events)]]
            [ div [id drawingId,  tabindex 0] (interface model)
            ]
        ]

-- User interface

option : Bool -> String -> String
option b str = if b then str else ""

activeClass : Maybe String -> String -> String
activeClass active choice = if active == Just choice then "active" else ""


makeTabs : Maybe String -> List (String, Html Msg) -> List (Html Msg)
makeTabs active tabs =
  let pane (name, inner) = div [classes ["sidebar"], hidden  (active /= Just name)]  [inner]

  in [div_ "tabs" [tabButtons active (List.map first tabs)]] ++ List.map pane tabs


tabButtons : Maybe String -> List String -> Html Msg
tabButtons active options =
    let tab str =
      let activate = if (Just str == active) then Nothing else Just str
      in a [classes ["nav-link", activeClass active str], onClick (ShowTab activate), href "#"] [text str]

  in nav [class "nav nav-pills bg-light rounded"] (List.map tab options)


div_ : String -> List (Html msg) -> Html msg
div_ name = div [class name]




interface : Model -> List (Html Msg)
interface model =
    let zoom amount = runCommand (Scene.zoomCentre model.scene amount)

        makeButton msg inner = button [type_ "button", class "btn btn-light", onClick msg] inner
        zoomButtons = div [class "btn-group-vertical zoom"]
          [ makeButton (zoom -25) [FA.plus]
          , makeButton (zoom 25) [FA.minus]
          ]

        scene = div (events model) [Scene.view Scene model.input model.scene]

        tabs = makeTabs model.activeTab [("Images", imageBar model), ("Options", optionsBar model)]

    in [scene, zoomButtons] ++ tabs


optionsBar : Model -> Html Msg
optionsBar model = div_ "card" [text "Hello world2 asdfasdfafdsfdas"]


makeSelect : (Int -> msg) -> Int -> List String -> Html msg
makeSelect tagger current options = let
  opt value name = Html.option [selected (current == value)] [text name]

    in Html.map tagger <| select [class "custom-select"] (List.indexedMap opt options)

datasetImages : Model -> List ImageInfo
datasetImages model = (Maybe.withDefault [] (Maybe.map (.images) model.dataset))

sortImages : Maybe String -> Int ->  List ImageInfo -> List ImageInfo
sortImages search sortMethod images = images

updateSort : Model -> Model
updateSort model = {model | sortedImages = datasetImages model}

imageBar : Model -> Html Msg
imageBar model =
  let item name inner = div_ "form-group" [label [] [text name, inner]]

      sortMethod = item "Sort by " (makeSelect SetSort (model.sortMethod) ["Name", "Annotated", "Non annotated"])
      imageSelect = Html.lazy2 imageSelector model.selectedFile model.sortedImages

  in div [class "card expand imagebar"]
        [div_ "card-body d-flex flex-column"
            [ Html.div   [class "form"] [sortMethod]
            , imageSelect]
        ]


imageSelector : Maybe String -> List ImageInfo ->  Html Msg
imageSelector active images =
  let selectRow info = tr [onClick (Select info.file), class (option (active == Just info.file) "table-active")]
        [ td [] (if info.annotated then [FA.edit] else [])
        , td [] [text info.file]
        ]

      heading str = th [] [text str]

  in div [class "scroll"]
    [ table [class "table table-sm"]
        [ thead [] [tr [] (List.map heading ["Edited", "Filename"])]
        , tbody [] (List.map selectRow images)
        ]
    ]
