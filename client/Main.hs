
module Main where

import Common

import qualified Data.Map as M

import           Miso hiding (for_)
import           Miso.String  (MisoString)
import qualified Miso.String  as S

import Miso.Subscription.Window
import Network.URI

import Scene.Types (Image(..), Command(..))
import qualified Scene
import qualified Input

import Types
import qualified Debug.Trace as Debug



data Action
  = HandleWebSocket (WebSocket ServerMsg)
  | SelectDoc DocName
  | ShowTab (Maybe MisoString)
  | Input Input.Event
  | Resize Dim

  | Scene [Command]
  -- | SendMessage ServerMsg
  -- | UpdateMessage MisoString
  | Id deriving (Eq, Show, Generic)

deriving instance Eq a => Eq (WebSocket a)
deriving instance Show a => Show (WebSocket a)

data NetworkState = Disconnected | Connected | Ready ClientId deriving (Show, Generic, Eq)

data Model = Model
  { dataset :: Dataset
  , network :: NetworkState
  , activeTab :: Maybe MisoString
  , scene :: Scene.Scene

  } deriving (Show, Generic, Eq)



initialModel :: Model
initialModel =  Model
  { dataset = Dataset defaultConfig []
  , network = Disconnected
  , activeTab = Nothing
  , scene = Scene.init
  }

main :: IO ()
main = do
  host <- fromMaybe "localhost" . fmap uriRegName . uriAuthority <$> getCurrentURI
  start host


start host = startApp App { initialAction = Id, ..} where
  model   = initialModel
  events  = defaultEvents <> [("wheel", False)]
  url = "ws://" <> host <> ":3000/ws"
  subs    =
    [ websocketSub (URL (S.pack url)) protocols HandleWebSocket
    , windowSub Resize
    ] <> Input.subs Input
  update  = updateModel
  view    = appView
  protocols = Protocols [ ]
  mountPoint = Nothing




imageInfo :: DocInfo -> DocName -> Image
imageInfo info name = Image (info ^. #imageSize) ("images/" <> S.toMisoString name)

openDocument :: DocName -> Document -> ClientId -> Model -> Model
openDocument name doc clientId model = maybe model open mInfo where
    mInfo     = M.lookup name (model ^. #dataset . #images)
    open info = model & #scene %~ Scene.setEditor editor
      where editor = Scene.makeEditor (imageInfo info name) (name, doc) clientId



_currentDoc :: Traversal' Model DocName
_currentDoc = #scene . Scene._Env . #docName

updateModel :: Action -> Model -> Effect Action Model
updateModel msg model = handle
    where
      handle = case msg of
        HandleWebSocket ws -> handleNetwork ws
        SelectDoc name -> model <# (send (ClientOpen name) >> pure Id)

        ShowTab maybeTab -> noEff (model & #activeTab .~ maybeTab)

        Scene cmds -> foldM runCommand model cmds

        Resize dim -> noEff $ model & #scene %~ Scene.resizeView (viewBox dim)
        Input e    ->
          updateInput e <$> foldM runCommand model (Scene.interact e (model ^. #scene))

        Id -> noEff model


      viewBox dim = Box (V2 0 0) (dimVector dim)
      updateInput e = #scene %~ Scene.updateInput e

        -- (SendMessage msg)    -> model <# do send msg >> pure Id
        --(UpdateMessage m)    -> noEff model { msg = Message m }

      runCommand :: Model -> Command -> Effect Action Model
      runCommand model cmd = model' <# toSend where
          model' = model & #scene %~ Scene.runCommand cmd
          toSend = case cmd of
            MakeEdit e -> do
               for_ (model ^? _currentDoc) $ \doc ->
                  send (ClientEdit doc e)
               pure Id

            _ -> pure Id


      handleNetwork :: WebSocket ServerMsg -> Effect Action Model
      handleNetwork (WebSocketMessage msg)     = handleMessage (model ^. #network) msg
      handleNetwork (WebSocketOpen) = noEff model { network = Connected }
      handleNetwork (WebSocketClose _ _ _) = noEff model { network = Disconnected }
      handleNetwork _ = noEff model

      handleMessage :: NetworkState -> ServerMsg -> Effect Action Model
      handleMessage Connected (ServerHello clientId dataset)  = model { network = Ready clientId, dataset = dataset}
        <# (const Id <$> case model ^? _currentDoc of
          Nothing       -> send (ClientNext Nothing)
          Just selected -> send (ClientOpen selected))

      handleMessage (Ready clientId) msg = case msg of
        ServerDocument docName document   -> noEff (openDocument docName document clientId model)
        ServerOpen maybeDoc otherId time  -> noEff model
        ServerEdit docName edit           -> noEff model

      handleMessage _ _ = error "message recieved in invalid state."






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
  div_ attrs
    [ div_ [classes_ ["expand horiz", cursorClass], style_ [("pointer-events", pointerEvents)]]
        [ div_ [id_ drawingId, tabindex_ 0] (interface model)
        ]
    ] where
      mCursor = model ^? #scene . Scene._Env . #interaction . #cursor
      (cursorClass, cursor, pointerEvents) = case mCursor of
        Nothing      -> ("", "auto", "auto")
        Just (cursor, True) -> ("cursor_lock", cursor, "none")
        Just (cursor, False) -> ("", cursor, "auto")

      attrs = [ Input.on' "wheel" emptyDecoder (const Id)
              , draggable_ False
              , style_ [("cursor", cursor)]
              ]

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
interface model@Model{..} =
  [ Scene <$> Scene.view scene
  , indicator network
  ] <> makeTabs activeTab [("Images", imageBar model)]



makeSelect :: Int -> [String] -> View Int
makeSelect current options = select_ [class_ "custom-select"] (opt <$> (zip [0..] options))
  where
    opt (value, name) = option_ [selected_ (current == value)] [text' name]

imageBar :: Model -> View Action
imageBar model@ (Model {..}) =
  div_ [class_ "card expand imagebar"]
        [div' "card-body d-flex flex-column"
            [ imageSelect]
        ]
  where    imageSelect = imageSelector (model ^? _currentDoc) $  M.toList (dataset ^. #images)


imageSelector :: Maybe DocName -> [(DocName, DocInfo)] ->  View Action
imageSelector active images =
  div_ [class_ "scroll"]
    [ table_ [class_ "table table-sm"]
        [ thead_ [] [tr_ [] (heading <$> ["Filename"])]
        , tbody_ [] (selectRow <$> images)
        ]
    ]
    where
      selectRow (name, info) = tr_ [onClick (SelectDoc name), classList_ [("table-active", active == Just name)]]
            [ td_ [] [text' name]
            --, td [] (if info.annotated then [FA.edit] else [])
            ]

      heading str = th_ [] [text str]




makeTabs :: Maybe MisoString -> [(MisoString, View Action)] -> [View Action]
makeTabs active tabs = [div' "tabs" [tabButtons active (fst <$> tabs)]] <> (pane <$> tabs)
  where pane (name, inner) = div_ [classes_ ["sidebar"], hidden_ (active /= Just name)]  [inner]

tabButtons :: Maybe MisoString -> [MisoString] -> View Action
tabButtons active options = nav_ [class_ "nav nav-pills bg-light rounded"] (tab <$> options)
  where
    tab option = a_ [classList_ [("nav-link", True), ("active", active == Just option)], onClick (activate option), href_ "#"] [text option]
    activate option = ShowTab $ if (Just option == active) then Nothing else (Just option)
