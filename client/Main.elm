module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Navigation as Nav

import Bootstrap.ButtonGroup as Bg
import Bootstrap.Button as Button

import Bootstrap.Card as Card
import Bootstrap.Grid as Grid

-- import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select

import Bootstrap.Accordion as Accordion

import Bootstrap.Table as Table
import Bootstrap.Tab as Tab
import FontAwesome.Web as FA


import Image exposing (Image)
import Network
import Drawing




import Scene.Types as Scene exposing (Scene, Action, Active(..), Command(..))

import Types exposing (Dataset, ImageInfo, Response(..), Request (..), Edit)
import Scene
-- import Input.Window as Window
-- import Vector exposing (Position)

import Util exposing (..)
import Debug

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
    { tabs   : Tab.State
    , accordion : Accordion.State
    , drawing : Drawing.Model
    , dataset : Maybe Dataset

    , selectedFile : Maybe String
    , location : Nav.Location

    , network : Network.State
    }


type Msg
    = Tabs Tab.State
    | Accordion Accordion.State
    | Drawing Drawing.Msg
    | Select String
    | ImageLoaded Image
    | UrlChange Nav.Location
    | Network Network.Msg





init : Nav.Location -> ( Model, Cmd Msg )
init loc = let
    (drawing, drawCmd) = Drawing.init
    (network, netCmd) = Network.init loc

    model = { tabs = Tab.initialState
            , accordion = Accordion.initialState
            , drawing = drawing
            , network = network
            , dataset = Nothing
            , selectedFile = Nothing
            , location = loc
            }
    cmds  = Cmd.batch
      [ Cmd.map Drawing drawCmd
      , Cmd.map Network netCmd
      , Network.request network.host ReqDataset
      ]

  in  (model, cmds)




imagePath : Dataset -> String -> String
imagePath dataset file = dataset.path ++ "/" ++ file


modifyScene : (Scene -> Scene) -> Model -> Model
modifyScene f model = {model | drawing = Drawing.modifyScene f model.drawing}


handleResponse : Maybe Response -> Model -> (Model, Cmd Msg)
handleResponse r model = case r of
  (Just (RespDataset d)) -> let m = {model | dataset = Just d} in
    case List.head d.images of
      Nothing -> noCmd m
      Just info -> selectImage (info.file) m

  (Just (RespOpen file doc)) -> noCmd (modifyScene (Scene.loadDocument doc) model)
  _ -> noCmd model


selectImage : String -> Model -> (Model, Cmd Msg)
selectImage file model = case model.dataset of
  Just dataset -> ({ model | selectedFile = Just file, drawing = Drawing.clear model.drawing },
    Cmd.batch [Image.loadImage (imagePath dataset file), Network.request model.network.host (ReqOpen file)] )
  Nothing -> noCmd model


withCmd : Cmd msg -> (a, Cmd msg) -> (a, Cmd msg)
withCmd cmd (a, cmds) = (a, Cmd.batch [cmd, cmds])



getEdits : Drawing.Msg -> Maybe Edit
getEdits msg = case msg of
  (Drawing.Scene (Scene.Run (MakeEdit e))) -> Just e
  _   -> Nothing


editRequest : Model -> Edit -> Cmd msg
editRequest model e = Network.request model.network.host (ReqEdit e)

reportEdits : Model -> Drawing.Msg -> Cmd msg
reportEdits model msg =  Maybe.withDefault Cmd.none <|
  Maybe.map (editRequest model) (getEdits msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Drawing msg   ->
      let (drawing, cmds) = Drawing.update msg model.drawing
      in ({ model | drawing = drawing}, Cmd.batch [Cmd.map Drawing cmds, reportEdits model msg])

    Tabs state -> noCmd { model | tabs = state }
    Accordion state -> noCmd { model | accordion = state }
    Select file -> selectImage file model

    ImageLoaded i -> noCmd { model | drawing = Drawing.setImage i model.drawing }
    UrlChange _ -> noCmd model
    Network m ->
      let (state, resp, cmd) = Network.update m model.network
      in withCmd  (Cmd.map Network cmd) (handleResponse resp {model | network = state})



subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Tab.subscriptions model.tabs Tabs
    , Drawing.subscriptions Drawing
    , Accordion.subscriptions model.accordion Accordion
    , Image.imageLoaded ImageLoaded
    , Network.subscriptions model.network Network
    ]



cols : List (Html msg) -> Html msg
cols xs = Grid.containerFluid [] [
    Grid.row [] (List.map (List.singleton >> Grid.col []) xs)
  ]


-- height scale = style [("height", scale)]
-- flex n =  style [("flex", toString n)]

cursorAttribs : Active -> (String, String, String)
cursorAttribs ma = case ma of
  Inactive      -> ("", "auto", "auto")
  Active action -> ("cursor_lock", action.cursor, "none")


sceneMsg : Scene.Msg -> Msg
sceneMsg = Drawing.Scene >> Drawing

drawings : Model -> List (Html Msg)
drawings model =
    let zoom amount = sceneMsg (Scene.Run (Scene.zoomCentre model.drawing.scene amount))

    in [ Bg.buttonGroup [ Bg.vertical, Bg.small, Bg.attrs [class "zoom"] ]
          [ Bg.button [ Button.secondary, Button.onClick (zoom -25)  ] [  FA.plus ]
          , Bg.button [ Button.secondary, Button.onClick (zoom 25) ] [  FA.minus ]
          ]
        ]


view : Model -> Html Msg
view model =
  let (cursor_class, cursor, pointer_events) = cursorAttribs model.drawing.scene.action in

    div [class "vert", draggable "false", style [("cursor", cursor)]]
        [ div [class ("expand horiz " ++ cursor_class), style [("pointer-events", pointer_events)]]
          [ sidebar model
          , (Drawing.view Drawing (drawings model) model.drawing)
          ]
        ]


tab : String -> String -> List (Html msg) -> Tab.Item msg
tab identifier title content =  Tab.item
    { id = identifier
    , link = Tab.link [] [ text title ]
    , pane = Tab.pane [] content
    }



item : String -> Select.Item msg
item v = Select.item [value v] [text v]

header : Table.THead msg
header = Table.simpleThead [ Table.th [] [], Table.th [] []  ]

toRow : Maybe String -> ImageInfo -> (String, Table.Row Msg)
toRow active image = let
    options = (Table.rowAttr <| onClick (Select image.file)) ::
      (if active == Just image.file then [Table.rowActive] else [])
    icon = if image.annotated then [FA.edit] else [FA.file_image_o]
    table = [ Table.td [] icon, Table.td [] [ text image.file ]]

  in (image.file, Table.tr options table)


imageSelector : Model -> List ImageInfo ->  Html Msg
imageSelector model images = div [class "select"] [Table.table
    { options = [ Table.small, Table.hover ] -- list of table options
    , thead = header
    , tbody = Table.keyedTBody [] (List.map (toRow model.selectedFile) images)
    }]



sidebar : Model -> Html Msg
sidebar model =
    div [class "sidebar"] [
      Tab.config Tabs
        -- |> Tab.left
        |> Tab.items [
            tab "images" "Images" [
              imageSelector model (case model.dataset of
                Nothing -> []
                Just dataset -> dataset.images)
            ],
            tab "instances" "Instances" [
              text "Cheese",
              accordion model
            ]
          ]
        |> Tab.view model.tabs
    ]



card : String -> String -> List (Card.BlockItem msg) -> Accordion.Card msg
card id title blocks = Accordion.card
  { id = id
  , options = []
  , header = Accordion.header [] <| Accordion.toggle [] [ text title ]
  , blocks = [Accordion.block [] blocks]
  }


accordion : Model -> Html Msg
accordion model =
    Accordion.config Accordion
        |> Accordion.withAnimation
        |> Accordion.cards
        [  card "card1" "Card 1"
            [ Card.text [] [ text "Lorem ipsum etc" ]
            ,  Card.custom <| Select.select [Select.attrs [size 5]] [item "foo.jpg", item "bar.jpg", item "baz.png", item "another", item "asdf"]
            ]
        , card "card2" "Card 2"
            [ Card.text [] [ text "Row row your boat.." ] ]
        ]
        |> Accordion.view model.accordion




-- If you use animations as above or you use dropdowns in your navbar you need to configure subscriptions too
