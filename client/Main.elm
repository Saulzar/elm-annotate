module Main exposing (main)

import Html exposing (..)
import Html.Lazy as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import FontAwesome.Web as FA

import Image exposing (Image)
import Network

import Scene.Types as Scene exposing (Scene, Action, Active(..), Command(..))
import Types exposing (Dataset, DocInfo, ServerMsg(..), ClientMsg (..), Edit(..), Document)
import Common exposing (DocName, ClientId, ObjId)

import Time exposing (Time)

import Input.Mouse as Mouse
import Input.Element as Element
import Scene.Action as Action

import Keyboard.Key as Key exposing (Key)

import Window
import Input
import Scene

import Json.Decode as Json
import Vector exposing (Position, Box, Size)

import Util exposing (..)
import Tuple exposing (..)

import Dict
import Debug

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


-- You need to keep track of the view state for the navbar in your model

type alias ReadyInfo  = { clientId : ClientId }
type NetworkState = Disconnected Int Int | Connected | Ready ReadyInfo

type alias Loading =
    { image  : Maybe Image
    , doc    : Maybe Document
    }

type Msg
    = Select String
    | ImageLoaded (DocName, Image)
    | Network Network.Msg
    | Scene (List Scene.Msg)

    | WindowResized
    | Input Input.Event
    | ViewSize Box

    | ShowTab (Maybe String)
    | SetSort Int
    | Ignore



type alias ImageInfo = (String, DocInfo)

type alias Model =
    { network    : NetworkState
    , hostname : String

    , dataset  : Dataset
    , scene   : Scene
    , loading : Loading

    , input  : Input.State

    -- User interface
    , selectedFile : Maybe String
    , activeTab  : Maybe String
    , sortMethod : Int

    }

type alias Flags = { hostname : String }

initLoading : Loading
initLoading = Loading Nothing Nothing

emptyDataset : Dataset
emptyDataset =
  { config = {extensions = []}
  , images = Dict.empty
  }


init : Flags -> ( Model, Cmd Msg )
init flags = let

    hostname = Network.websocketHost flags.hostname
    model = { network = Disconnected 0 0
            , hostname = flags.hostname

            , scene = Scene.init
            , loading = initLoading
            , input = Input.init

            , selectedFile = Nothing
            , sortMethod = 0
            , activeTab = Just "Images"
            , dataset = emptyDataset
            }

    cmds  =
      [ Network.connect hostname
      , Element.askGeometry drawingId
      ]

  in  model ! cmds


imagePath : Dataset -> String -> String
imagePath dataset file = "images" ++ "/" ++ file


modifyScene : (Scene -> Scene) -> Model -> Model
modifyScene f model = {model | scene = f model.scene}

handleMessage : ServerMsg -> Model -> (Model, Cmd Msg)
handleMessage msg model = case msg of
  ServerHello clientId dataset -> noCmd {model | network = Ready {clientId = clientId}, dataset = dataset}
  _ -> Debug.crash "handleMessage: not implemented"


handleNetwork : Network.Msg -> Model -> (Model, Cmd Msg)
handleNetwork r model = case r of
  Network.Open            -> noCmd { model | network = Connected }
  Network.Message msg     -> handleMessage msg model
  Network.Close           -> case model.network of
    Disconnected retries _ -> connectIn (retries + 1) (Basics.min 16 (2 ^ retries)) model
    _ -> { model | network = Disconnected 0 0 } ! [Network.connect model.hostname]

  Network.Retry -> case model.network of
    Disconnected retries time -> connectIn retries (time - 1) model
    _                         -> noCmd model

  Network.Error e -> Debug.crash e

connectIn : Int -> Int -> Model -> (Model, Cmd Msg)
connectIn retries time model =
  let cmd = if time > 0
        then delay Network.Retry (1 * Time.second)
        else Network.connect model.hostname
  in { model | network = Disconnected retries time }  ! [Cmd.map Network cmd]

getClientId : NetworkState -> Maybe ClientId
getClientId ns = case ns of
  Ready i -> Just i.clientId
  _ -> Nothing



whenReady : Model -> (ReadyInfo -> Cmd msg) -> Cmd msg
whenReady model f = case model.network of
  Ready r -> f r
  _ -> Cmd.none


selectImage : String -> Model -> (Model, Cmd Msg)
selectImage file model =
    let model1 = { model | loading = initLoading  }
        url = "images/" ++ file
        cmd = whenReady model1 <| \_ -> Cmd.batch [Image.loadImage (file, url),  Network.sendMsg (ClientOpen file)]
    in (model1, cmd)


editRequest : DocName -> Edit -> Cmd msg
editRequest name e = Network.sendMsg (ClientEdit name e)


sceneMsg : Scene.Msg -> Model -> (Model, Cmd Msg)
sceneMsg msg model = case msg of
  Scene.Run (MakeEdit e) -> Debug.crash "fixme"
  _ -> noCmd (modifyScene (Scene.update msg) model)


updateLoading : Model -> (Loading -> Loading) -> Model
updateLoading model f =
  let loading  = f model.loading
  in case Maybe.map3 Scene.load (getClientId model.network) loading.doc loading.image of
    Nothing   -> {model | loading = loading}
    Just f   -> {model | scene = f model.scene, loading = initLoading}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Select file -> selectImage file model

    Network m -> handleNetwork m model

    ViewSize box  -> noCmd (modifyScene (Scene.setBounds box) model)
    WindowResized -> (model, Element.askGeometry drawingId)
    ImageLoaded (name, info) -> noCmd <| updateLoading model <| \loading ->
      if Just name == model.selectedFile then { loading | image = Just info} else loading

    Scene msgs -> foldUpdate sceneMsg model msgs
    Input event ->
      let input = Input.update event model.input
          msgs = globalInput (event, input) model.scene
      in foldUpdate sceneMsg {model | input = input}  msgs

    ShowTab option -> noCmd {model | activeTab = option}
    SetSort method -> noCmd {model | sortMethod = method}
    Ignore -> noCmd model




subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Image.imageLoaded ImageLoaded
    , Network.subscriptions Network

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


indicator : NetworkState -> Html Msg
indicator state = div_ "indicator" <| case state of
  Disconnected n time -> [ul [class "list-unstyled"]
    [ li [] [ text "Disconnected ", span [class "text-danger"] [FA.warning]]
    -- , li [] [ text ("Reconnecting.. " ++ toString time)]
    ]]
  Connected           -> [text "Loading ", span [class "text-info"] [FA.refresh]]
  Ready _ -> [text "Ready ", span [class "text-success"] [FA.check]]



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

    in [scene, zoomButtons, indicator (model.network)] ++ tabs


optionsBar : Model -> Html Msg
optionsBar model = div_ "card" [text "Hello world2 asdfasdfafdsfdas"]


makeSelect : (Int -> msg) -> Int -> List String -> Html msg
makeSelect tagger current options = let
  opt value name = Html.option [selected (current == value)] [text name]

    in Html.map tagger <| select [class "custom-select"] (List.indexedMap opt options)


getImages : Model -> List ImageInfo
getImages model = Dict.toList model.dataset.images


imageBar : Model -> Html Msg
imageBar model =
  let item name inner = div_ "form-group" [label [] [text name, inner]]

      sortMethod = item "Sort by " (makeSelect SetSort (model.sortMethod) ["Name", "Annotated", "Non annotated"])
      imageSelect = Html.lazy2 imageSelector model.selectedFile (getImages model)

  in div [class "card expand imagebar"]
        [div_ "card-body d-flex flex-column"
            [ Html.div   [class "form"] [sortMethod]
            , imageSelect]
        ]


imageSelector : Maybe String -> List ImageInfo ->  Html Msg
imageSelector active images =
  let selectRow (name, info) = tr [onClick (Select name), class (option (active == Just name) "table-active")]
        [ td [] [text name]
        --, td [] (if info.annotated then [FA.edit] else [])
        ]

      heading str = th [] [text str]

  in div [class "scroll"]
    [ table [class "table table-sm"]
        [ thead [] [tr [] (List.map heading ["Edited", "Filename"])]
        , tbody [] (List.map selectRow images)
        ]
    ]
