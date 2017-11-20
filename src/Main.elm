module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Bootstrap.ButtonGroup as Bg
import Bootstrap.Button as Button

import Bootstrap.Card as Card
import Bootstrap.Grid as Grid

-- import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select

import Bootstrap.Accordion as Accordion

import Bootstrap.Table as Table
import Bootstrap.Tab as Tab
import Bootstrap.Navbar as Navbar

import Drawing
import Image exposing (Image)
import Dataset as D

import FontAwesome.Web as FA

import Scene.Types as Scene exposing (Action, Active(..), Command(..))
import Scene
-- import Input.Window as Window
-- import Vector exposing (Position)

import Util exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


-- You need to keep track of the view state for the navbar in your model

type alias Model =
    { navbar : Navbar.State
    , tabs   : Tab.State
    , accordion : Accordion.State
    , drawing : Drawing.Model
    , dataset : D.Dataset

    , selectedFile : Maybe String
    }


type Msg
    = Navbar Navbar.State
    | Tabs Tab.State
    | Accordion Accordion.State
    | Drawing Drawing.Msg
    | Select String
    | ImageLoaded Image





init : ( Model, Cmd Msg )
init = let
    (navState, navCmd) = Navbar.initialState Navbar
    (drawingState, drawingCmd) = Drawing.init
    model = {
      navbar = navState, tabs = Tab.initialState, accordion = Accordion.initialState, drawing = drawingState,
      dataset = D.dataset, selectedFile = Nothing
    }
    cmds  = Cmd.batch [navCmd, Cmd.map Drawing drawingCmd]
  in  (model, cmds)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
        Navbar state -> noCmd { model | navbar = state }
        Drawing msg   -> let (drawing, cmds) = Drawing.update msg model.drawing in ({ model | drawing = drawing}, Cmd.map Drawing cmds)
        Tabs state -> noCmd { model | tabs = state }
        Accordion state -> noCmd { model | accordion = state }
        Select file -> let url = model.dataset.path ++ "/" ++ file in
           ({ model | selectedFile = Just file}, Image.loadImage url)

        ImageLoaded i -> noCmd { model | drawing = Drawing.setImage i model.drawing }





subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Navbar.subscriptions model.navbar Navbar
    , Tab.subscriptions model.tabs Tabs
    , Drawing.subscriptions Drawing
    , Accordion.subscriptions model.accordion Accordion
    , Image.imageLoaded ImageLoaded
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


-- select options = select
-- selectFrom : List String -> Html Msg
-- selectFrom opts = select [multiple True, class "form-control"]
--   (List.map (text >> List.singleton >> option []) opts)


item : String -> Select.Item msg
item v = Select.item [value v] [text v]



-- targetSelectedIndex : Json.Decoder Int
-- targetSelectedIndex =
--     Json.at [ "target", "selectedIndex" ] Json.int
--
--
-- onSelect : (Int -> msg) -> Html.Attribute msg
-- onSelect msg = on "change" (Json.map msg targetSelectedIndex)
--
--
-- selectImage : List D.Image -> Html Msg
-- selectImage images = Select.select [Select.attrs [size 5] ]
--   <| List.map imageEntry images


-- imageEntry : D.Image -> Select.Item Msg
-- imageEntry image = Select.item [value image.file] <|
--     [text (icon ++ image.file)]

header : Table.THead msg
header = Table.simpleThead
    [ Table.th [] []
    , Table.th [] []
    ]


toRow : Maybe String -> D.Image -> (String, Table.Row Msg)
toRow active image = let
    options = (Table.rowAttr <| onClick (Select image.file)) ::
      (if active == Just image.file then [Table.rowActive] else [])
    icon = if image.annotated then [FA.edit] else [FA.file_image_o]
    table = [ Table.td [] icon, Table.td [] [ text image.file ]]

  in (image.file, Table.tr options table)


selectImage : Model -> Html Msg
selectImage model = div [class "scroll"] [Table.table
    { options = [ Table.small, Table.hover, Table.attr <| class "image_select" ] -- list of table options
    , thead = header
    , tbody = Table.keyedTBody [] (List.map (toRow model.selectedFile) model.dataset.images)
    }]



sidebar : Model -> Html Msg
sidebar model =
    div [class "sidebar"] [
      Tab.config Tabs
        -- |> Tab.left
        |> Tab.items [
            tab "images" "Images" [
              selectImage model
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
