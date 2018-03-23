
module Main where

import Common

import qualified Data.Map as M
import qualified Data.Text as T

import           Miso hiding (for_, at)
import           Miso.String  (MisoString)
import qualified Miso.String  as S

import Miso.Subscription.Window
import Miso.Subscription.History
import Network.URI

import Control.Concurrent

import Scene.Types (Image(..))
import qualified Scene
import qualified Input

import qualified Web.KeyCode as Key

import Types
import qualified Debug.Trace as Debug

import Control.Lens (makePrisms, matching)
import Data.JSString.Int (decimal)

import Data.Time.Format.Human (humanReadableTime')

data Popup = SubmitMenu deriving (Eq, Ord, Enum, Show)

data Action
  = HandleWebSocket (WebSocket ServerMsg)
  | SelectDoc MisoString
  | Timer UTCTime
  | ShowTab (Maybe MisoString)
  | Input Input.Event
  | Resize Dim

  -- | SendMessage ServerMsg
  -- | UpdateMessage MisoString
  | Submit ImageCat
  | ShowPopup Popup
  | CancelPopup
  | Id deriving (Eq, Show, Generic)

deriving instance Eq a => Eq (WebSocket a)
deriving instance Show a => Show (WebSocket a)

data NetworkState = Disconnected | Connected | Ready ClientId deriving (Show, Generic, Eq)


data Model = Model
  { dataset   :: Collection
  , network   :: NetworkState
  , activeTab :: Maybe MisoString
  , scene     :: Scene.Scene
  , popup     :: Maybe Popup
  , selected  :: Maybe DocName

  , time :: UTCTime

  } deriving (Show, Generic, Eq)



initialModel :: UTCTime -> Model
initialModel t =  Model
  { dataset = Collection defaultConfig []
  , network = Disconnected
  , activeTab = Nothing
  , scene = Scene.init
  , popup = Nothing
  , selected = Nothing
  , time = t

  }

foreign import javascript unsafe "$r = window.location.hostname;"
  getHostName :: IO MisoString

foreign import javascript unsafe "$r = window.location.href;"
  getHref :: IO MisoString

getURIHref :: IO URI
getURIHref = do
  href <- S.fromMisoString <$> getHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " <> href
    Just uri -> return uri

every :: Int -> IO action -> Sub action Model
every microSeconds action getm sink =
  void . forkIO . forever $ do
    threadDelay microSeconds
    action >>= sink


main :: IO ()
main = do
  uri <- getURIHref
  let host = maybe "" uriRegName (uriAuthority uri)
  t <- getCurrentTime
  start t $ if null host then "localhost" else host

start t host = startApp App { initialAction = Id, ..} where
  model   = initialModel t
  events  = defaultEvents <> [("wheel", False)]
  url = "ws://" <> host <> ":3000/ws"
  subs    =
    [ websocketSub (URL (S.pack url)) protocols HandleWebSocket
    , windowSub Resize
    , uriSub (SelectDoc . fromString . drop 1 . uriFragment)
    , every second (Timer <$> getCurrentTime)

    ] <> Input.subs Input
  update  = updateModel
  view    = appView
  protocols = Protocols [ ]
  mountPoint = Nothing

  second = 1000000




imageInfo :: DocInfo -> DocName -> Image
imageInfo info name = Image (info ^. #imageSize) ("images/" <> S.toMisoString name)

openDocument :: DocName -> DocInfo -> Document -> ClientId -> Model -> Model
openDocument name info doc clientId model = model
      & #scene %~ Scene.setEditor editor
    where editor = Scene.makeEditor (imageInfo info name) (name, doc) clientId


_currentDoc :: Traversal' Model DocName
_currentDoc = #scene . Scene._Env . #name

isReady :: Model -> Bool
isReady model = case model ^. #network of
  Ready _ -> True
  _       -> False


updateModel :: Action -> Model -> Effect Action Model
updateModel msg model = handle where
  handle = case msg of
    HandleWebSocket ws -> handleNetwork ws
    SelectDoc name -> (model & #selected .~ mDoc) <# do
      forM_ mDoc $ \doc ->
        when (isReady model && model ^. #selected /= Just doc) $
          send (ClientOpen doc)
      return Id
        where mDoc = if S.null name
              then Nothing
              else Just $ fromString $ S.unpack name

    ShowTab maybeTab -> noEff (model & #activeTab .~ maybeTab)

    Resize (h, w) -> noEff $ model & #scene %~ Scene.resizeView (viewBox (w, h))
    Input e    -> handleGlobal e (model & #scene .~ scene') <#
      (traverse_ (sendCommand model) cmds >> pure Id)
        where (scene', cmds) = Scene.interact e (model ^. #scene)

    ShowPopup p   -> noEff $ model & #popup .~ Just p
    CancelPopup   -> noEff $ model & #popup .~ Nothing

    Timer t -> noEff $ model & #time .~ t

    Submit cat -> case (model ^? _currentDoc, isReady model) of
      (Just doc, True) -> model <# (send (ClientSubmit doc cat) >> return Id)
      _ -> noEff model

    _ -> noEff model





  viewBox dim = Box (V2 0 0) (dimVector dim)
  updateInput e = #scene %~ Scene.updateInput e

  cancelPopup model = model & #popup .~ (Nothing :: Maybe Popup)

  handleGlobal e model = case e of
    Input.KeyDown Key.Escape -> cancelPopup model
    Input.KeyDown Key.Enter  -> cancelPopup model
    _ -> model

    -- (SendMessage msg)    -> model <# do send msg >> pure Id
    --(UpdateMessage m)    -> noEff model { msg = Message m }

  sendCommand :: Model -> DocCmd -> IO ()
  sendCommand model cmd  =
      for_ (model ^? _currentDoc) $ \doc -> send (ClientCmd doc cmd)


  handleNetwork :: WebSocket ServerMsg -> Effect Action Model
  handleNetwork (WebSocketMessage msg)     = handleMessage (model ^. #network) msg
  handleNetwork (WebSocketOpen) = noEff model { network = Connected }
  handleNetwork (WebSocketClose _ _ _) = noEff model { network = Disconnected }
  handleNetwork _ = noEff model

  handleMessage :: NetworkState -> ServerMsg -> Effect Action Model
  handleMessage Connected (ServerHello clientId dataset)  = model { network = Ready clientId, dataset = dataset}
    <# (const Id <$> case model ^. #selected of
      Nothing  -> send (ClientNext Nothing)
      Just doc -> send (ClientOpen doc))

  handleMessage (Ready clientId) msg = case msg of
    ServerDocument docName info document   -> openDocument docName info document clientId model <# do
      uri <- getURIHref
      let frag = "#" <> T.unpack docName
      when (uriFragment uri /= frag) $
        pushURI (uri {uriFragment = frag})
      return Id

    ServerUpdateInfo docName info -> noEff $
      model & #dataset . #images . at docName .~ Just info

    ServerOpen maybeDoc otherId time  -> noEff model
    ServerCmd docName edit           -> noEff model
    _ -> noEff model

  handleMessage _ _ = error "message recieved in invalid state."



text' str = text $ S.toMisoString str

drawingId :: MisoString
drawingId = "drawing"

draggable_ :: Bool -> Attribute action
draggable_ = boolProp "draggable"

tabindex_ :: Int -> Attribute action
tabindex_ = intProp "tabindex"

datatoggle_ = textProp "data-toggle"
ariahaspopup_ = boolProp "aria-haspopup"
role_ = textProp "role"
ariaexpanded_ = boolProp "aria-expanded"
arialabelledby_ = textProp "aria-labelledby"


classes_ :: [MisoString] -> Attribute action
classes_ = class_ . S.concat . intersperse " "


appView :: Model -> View Action
appView model =
  div_ attrs
    [ div_ [classes_ ["expand horiz", cursorClass]]
        [ div_ [id_ drawingId, tabindex_ 0] (interface model)
        ]
    ] where
      mCursor = model ^? #scene . Scene._Env . #interaction . #cursor
      (cursorClass, cursor) = case mCursor of
        Nothing      -> ("", "auto")
        Just (cursor, True) -> ("cursor_lock", cursor)
        Just (cursor, False) -> ("", cursor)

      attrs = [ Input.on' "wheel" emptyDecoder (const Id)
              , draggable_ False
              , style_ [("cursor", cursor)]
              ] <> [onClick CancelPopup | isJust (model ^. #popup)]

div' :: MisoString -> [View action] -> View action
div' c = div_ [class_ c]

icon :: MisoString -> View action
icon name = i_ [class_ ("fa fa-" <> name)] []

iconLarge :: MisoString -> View action
iconLarge name = i_ [class_ ("fa fa-3x fa-" <> name)] []


indicator :: NetworkState -> View action
indicator state = div' "indicator" $ case state of
   Disconnected -> [ul_ [class_ "list-unstyled"]
    [ li_ [] [ text "Disconnected ", span_ [class_ "text-danger"] [icon "warning"]]
    ]]
   Connected -> [text "Loading ", span_ [class_ "text-info"] [icon "refresh"]]
   Ready _   -> [text "Ready ", span_ [class_ "text-success"] [icon "check"]]

submitMenu :: Model -> View Action
submitMenu model = div' "submit"
  [ dropupSplit (model ^. #popup == Just SubmitMenu) SubmitMenu (Submit Train, "Submit training")
      [ MenuItem (Submit Test, "For testing")
      , MenuItem (Submit Hold, "Hold image")
      ]
  ]

interface :: Model -> [View Action]
interface model@Model{..} =
  [ Input <$> Scene.render scene
  , indicator network
  , submitMenu model
  -- , imageBar model
  ] <> makeTabs activeTab
    [ ("Images", imageSelect model)
    ]

makeSelect :: Int -> [String] -> View Int
makeSelect current options = select_ [class_ "custom-select"] (opt <$> (zip [0..] options))
  where
    opt (value, name) = option_ [selected_ (current == value)] [text' name]

imageSelect :: Model -> View Action
imageSelect model@(Model {..}) =
  div_ [class_ "card expand imageselect"]
        [div' "card-body d-flex flex-column"
            [ imageSelect]
        ]
  where    imageSelect = imageSelector (model ^. #selected) (model ^. #time) $  M.toList (dataset ^. #images)

imageBar :: Model -> View Action
imageBar model = nav_ [class_ "nav navbar navbar-expand-lg bg-light rounded"]
  [ span_ [class_ "navbar-text mr-auto ml-auto"] ["Testing 123"]
  , ul_ [class_ "navbar-nav"]
      [ navLink (Id, [text "Submit "])
      ]
  ]

data MenuItem = MenuItem (Action, MisoString) | Divider

navLink :: (Action, [View Action]) -> View Action
navLink (action, content) = li_ [class_ "nav-item"] [a_ [class_ "nav-link", onClick action ] content]

mBool :: Monoid a => Bool -> a -> a
mBool b x = if b then x else mempty

navMenu :: Bool -> Popup -> MisoString -> [MenuItem] -> View Action
navMenu shown popup str items  = li_ [class_ ("nav-item dropdown " <> mBool shown "show")]
  [ a_  attrs [text str]
  , menu shown items
  ] where
    attrs = [onClick (ShowPopup popup) | not shown] <> [ class_ "nav-link dropdown-toggle"
             , datatoggle_ "dropdown", role_ "button", ariahaspopup_ True, ariaexpanded_ shown]

dropdownSplit = dropdownSplit' True
dropupSplit = dropdownSplit' False

dropdownSplit' :: Bool -> Bool -> Popup -> (Action, MisoString) -> [MenuItem] -> View Action
dropdownSplit' isDown shown popup (action, str) items = div_ [class_ ("btn-group " <> if isDown then "dropdown" else "dropup")]
  [ button_ [type_ "button", class_ "btn btn-light", onClick action] [text str]
  , button_ attrs []
  , menu shown items
  ]
  where
    attrs = [ onClick (ShowPopup popup) | not shown] <> [ class_ "btn btn-light dropdown-toggle dropdown-toggle-split"
            , datatoggle_ "dropdown", role_ "button", ariahaspopup_ True, ariaexpanded_ shown]


menu :: Bool -> [MenuItem] -> View Action
menu shown items = div_ [class_ ("dropdown-menu " <> mBool shown "show")] (link <$> items)
  where

    link (MenuItem (action, str)) = a_ [class_ "dropdown-item", onClick action ] [text str]
    link Divider = div_ [class_ "dropdown-divider"] []

showMiso = S.toMisoString . show

imageSelector :: Maybe DocName -> UTCTime ->  [(DocName, DocInfo)] ->  View Action
imageSelector active currentTime images =
  div_ [class_ "scroll"]
    [ table_ [class_ "table table-hover table-sm"]
        [ thead_ [] [tr_ [] (heading <$> ["File", "Size", "Status", "Modified"])]
        , tbody_ [] (selectRow . over _1 S.toMisoString <$> images)
        ]
    ]
    where
      active' = S.toMisoString <$> active
      selectRow (name, DocInfo{..}) =
        tr_ [classList_ [("table-active", active' == Just name)], onClick (SelectDoc name)]
          --[ a_ [href_ ("#" <> name)]
          (td_ [] <$>
              [ [text name]
              , [text (toDim imageSize)]
              , [text (showMiso category)]
              , [text (fromMaybe "" (showModified <$> modified))]
              ])

      heading str = th_ [] [text str]
      toDim (w, h) = decimal w <> "×" <> decimal h
      showModified modified = S.toMisoString (humanReadableTime' currentTime modified)



makeTabs :: Maybe MisoString -> [(MisoString, View Action)] -> [View Action]
makeTabs active tabs = [div' "tabs" [tabButtons active (fst <$> tabs)]] <> (pane <$> tabs)
  where pane (name, inner) = div_ [classes_ ["sidebar"]]  [inner | active == Just name]

tabButtons :: Maybe MisoString -> [MisoString] -> View Action
tabButtons active options = nav_ [class_ "nav nav-pills bg-light rounded"] (tab <$> options)
  where
    tab option = a_ [ classList_ [("nav-link", True), ("active", active == Just option)]
                    , onClick (activate option)] [text option]

    activate option = ShowTab $
      if Just option == active then Nothing else Just option


linkable :: Attribute action
linkable = href_ "javascript:void(0)"
