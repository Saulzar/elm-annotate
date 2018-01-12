
module Main where

import Data.Monoid
import           Data.Aeson
import           GHC.Generics
import           Data.Bool
import qualified Data.Map as M
import Data.Foldable

import Data.List (intersperse)

import           Miso
import           Miso.String  (MisoString)
import qualified Miso.String  as S

import Data.Maybe
import Network.URI
import Types

import Control.Lens


data Action
  = HandleWebSocket (WebSocket ServerMsg)
  | Select DocName
  | ShowTab (Maybe MisoString)
  -- | SendMessage ServerMsg
  -- | UpdateMessage MisoString
  | Id

data NetworkState = Disconnected | Connected | Ready ClientId deriving (Show, Generic, Eq)

data Model = Model
  { dataset :: Dataset
  , network :: NetworkState

  , selected :: Maybe DocName
  , activeTab :: Maybe MisoString

  } deriving (Show, Generic, Eq)




initialModel :: Model
initialModel =  Model
  { dataset = Dataset defaultConfig []
  , network = Disconnected
  , selected = Nothing
  , activeTab = Nothing
  }

main :: IO ()
main = do
  host <- fromMaybe "localhost" . fmap uriRegName . uriAuthority <$> getCurrentURI
  start host


start host = startApp App { initialAction = Id, ..} where
  model   = initialModel
  events  = defaultEvents
  url = "ws://" ++ host ++ ":3000/ws"
  subs    = [ websocketSub (URL (S.pack url)) protocols HandleWebSocket ]
  update  = updateModel
  view    = appView
  protocols = Protocols [ ]
  mountPoint = Nothing





updateModel :: Action -> Model -> Effect Action Model
updateModel msg model = handle
    where
      handle = case msg of
        HandleWebSocket ws -> handleNetwork ws
        Select name -> noEff (model & #selected .~ Just name)
        ShowTab maybeTab -> noEff (model & #activeTab .~ maybeTab)
        Id -> noEff model

        -- (SendMessage msg)    -> model <# do send msg >> pure Id
        --(UpdateMessage m)    -> noEff model { msg = Message m }


      handleNetwork :: WebSocket ServerMsg -> Effect Action Model
      handleNetwork (WebSocketMessage m)     = handleMessage m
      handleNetwork (WebSocketOpen) = noEff model { network = Connected }
      handleNetwork (WebSocketClose _ _ _) = noEff model { network = Disconnected }
      handleNetwork _ = noEff model

      handleMessage :: ServerMsg -> Effect Action Model
      handleMessage (ServerHello clientId dataset) = noEff model { network = Ready clientId, dataset = dataset}
      handleMessage _ = noEff model




cursorAttribs :: Maybe MisoString -> (MisoString, MisoString, MisoString)
cursorAttribs ma = case ma of
  Nothing      -> ("", "auto", "auto")
  Just cursor -> ("cursor_lock", cursor, "none")



text' str = text $ S.toMisoString str

drawingId :: MisoString
drawingId = "drawing"

draggable_ :: Bool -> Attribute action
draggable_ = boolProp "draggable"

tabindex_ :: Int -> Attribute action
tabindex_ = intProp "tabindex"

classes_ :: [MisoString] -> Attribute action
classes_ = class_ . S.concat . intersperse " "

appView :: Model -> View Action
appView model =
  div_ [draggable_ False, style_ [("cursor", cursor)]]
    [ div_ [classes_ ["expand horiz", cursorClass], style_ [("pointer-events", pointerEvents)]]
        [ div_ [id_ drawingId, tabindex_ 0] (interface model)
        ]
    ] where
      (cursorClass, cursor, pointerEvents) = cursorAttribs Nothing


div' :: MisoString -> [View action] -> View action
div' c = div_ [class_ c]

icon :: MisoString -> View action
icon name = i_ [class_ ("fa fa-" <> name)] []

indicator :: NetworkState -> View action
indicator state = div' "indicator" $ case state of
   Disconnected -> [ul_ [class_ "list-unstyled"]
    [ li_ [] [ text "Disconnected ", span_ [class_ "text-danger"] [icon "warning"]]
    ]]
   Connected -> [text "Loading ", span_ [class_ "text-info"] [icon "refresh"]]
   Ready _   -> [text "Ready ", span_ [class_ "text-success"] [icon "check"]]

interface :: Model -> [View Action]
interface model =
  [ indicator $ model ^. #network
  ] ++ makeTabs (model ^. #activeTab) [("Images", imageBar model)]



makeSelect :: Int -> [String] -> View Int
makeSelect current options = select_ [class_ "custom-select"] (map opt (zip [0..] options))
  where
    opt (value, name) = option_ [selected_ (current == value)] [text' name]

imageBar :: Model -> View Action
imageBar model@ (Model {..}) =
  div_ [class_ "card expand imagebar"]
        [div' "card-body d-flex flex-column"
            [ imageSelect]
        ]
  where    imageSelect = imageSelector selected $ M.toList (dataset ^. #images)


imageSelector :: Maybe String -> [(DocName, DocInfo)] ->  View Action
imageSelector active images =
  div_ [class_ "scroll"]
    [ table_ [class_ "table table-sm"]
        [ thead_ [] [tr_ [] (heading <$> ["Filename"])]
        , tbody_ [] (selectRow <$> images)
        ]
    ]
    where
      selectRow (name, info) = tr_ [onClick (Select name), classList_ [("table-active", active == Just name)]]
            [ td_ [] [text' name]
            --, td [] (if info.annotated then [FA.edit] else [])
            ]

      heading str = th_ [] [text str]




makeTabs :: Maybe MisoString -> [(MisoString, View Action)] -> [View Action]
makeTabs active tabs = [div' "tabs" [tabButtons active (fst <$> tabs)]] ++ (pane <$> tabs)
  where pane (name, inner) = div_ [classes_ ["sidebar"], hidden_ (active /= Just name)]  [inner]

tabButtons :: Maybe MisoString -> [MisoString] -> View Action
tabButtons active options = nav_ [class_ "nav nav-pills bg-light rounded"] (tab <$> options)
  where
    tab option = a_ [classList_ [("nav-link", True), ("active", active == Just option)], onClick (activate option), href_ "#"] [text option]
    activate option = ShowTab $ if (Just option == active) then Nothing else (Just option)
