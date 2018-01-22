module Scene
  ( toLocal

  , module Scene
  , Scene(..)
  , Action(..)
  ) where

import Common

import Miso (View, Attribute, on, div_)
import qualified Miso.String as S

import qualified Scene.Settings as Settings
import qualified Scene.Viewport as Viewport
import qualified Scene.Interaction as I
import qualified Input as Input

import qualified Document as Doc

import Input (Button(..))
import Scene.Viewport (Viewport, toLocal)

import Control.Lens (prism', Prism')

import Types
import Scene.Types
import Svg

-- import Debug.Trace

data Action
  = Resize Box
  | Run Command
  | Input Input.Event
    deriving (Show, Eq, Generic)




init :: Scene
init = Scene
  { settings = Settings.init
  , viewport = Viewport.init
  , input = Input.init
  , editor = Nothing
  }

makeEditor :: Image -> (DocName, Document) -> ClientId -> Editor
makeEditor background (docName, doc) clientId = Editor {..} where
    nextId     = ObjId (i, clientId)
    selection  = []
    interaction = Nothing
    i = fromMaybe 0 (fst . unObj <$> Doc.maxId doc)



setEditor :: Editor -> Scene -> Scene
setEditor e = (#editor .~ Just e) . (#viewport . #size .~ toSize dim) where
    toSize (w, h) = V2 (fromIntegral w) (fromIntegral h)
    dim = e ^. #background . #size

resizeView :: Box -> Scene -> Scene
resizeView b = #viewport . #bounds .~ b


updateInput :: Input.Event -> Scene -> Scene
updateInput e = #input %~ Input.update e

interact :: Input.Event -> Scene -> [Command]
interact e scene = fromMaybe [] $ do
  env         <- maybeEnv scene
  update <- env ^? #interaction . traverse . #update
  return (update e env)


runCommand :: Command -> Scene -> Scene
runCommand cmd = over _Env $ case cmd of
   Select ids        -> #selection .~ ids
   Pan pos page      -> #viewport %~ Viewport.panView pos page
   Zoom amount pos   -> #viewport %~ Viewport.zoomView amount pos

   ScaleBrush amount -> #settings %~ Settings.scaleBrush amount
   MakeEdit name edit -> \scene -> if scene ^. #docName == name
     then scene & #doc %~ Doc.applyEdit edit
     else scene

   Start interaction -> #interaction .~ Just interaction
   End               -> #interaction .~ Nothing



svgImage :: Image -> View action
svgImage Image{..} = image_ [xlinkHref_ (S.pack source), x_ 0, y_ 0, width_ w, height_ h ] []
  where (V2 w h) = toVector size


localPosition :: Env -> Position
localPosition Env{..} = toLocal viewport (input ^. #mouse)

events :: Env -> [Attribute [Command]]
events env =
  [ on "mousedown" Input.buttonDecoder $ \case
        LeftButton -> [Select [], Start (I.pan (localPosition env))]
        _          -> []

  , on "wheel" Input.wheelDecoder (\delta ->
        [Zoom delta (localPosition env)])
  ]

view :: Scene -> View [Command]
view scene = case maybeEnv scene of
  Nothing  -> div_ [] []
  Just env -> div_ (events env) $ pure $
      Viewport.view (scene ^. #viewport)  $
        [ svgImage (env ^. #background)
        ]


maybeEnv :: Scene -> Maybe Env
maybeEnv Scene{..}
  | Just Editor{..} <- editor = Just Env{..}
  | otherwise                 = Nothing

toScene :: Env -> Scene
toScene Env{..} = Scene{..}  where
  editor = Just Editor{..}

_Env :: Prism' Scene Env
_Env = prism' toScene maybeEnv
